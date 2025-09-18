# Shiny dashboard for CS-3+ ASIC Bringup Production Data

library(shiny)
library(shinydashboard)
library(RPostgres)
library(pool)
library(tidyverse)
library(scales)
library(ggTimeSeries)
library(plotly)
library(glue)
library(lubridate)

# Set up the DB pool for CBDA queries
cbdaPool <- dbPool(
  drv = RPostgres::Postgres(),
  host = 'cbda-postgres.cerebras.aws',
  # host = 'cbda-postgres-dev.cerebras.aws',
  dbname = 'cbdapg',
  user = 'asic_bring_up_writer',
  password = 'writer3896'
)

onStop(function() {
  poolClose(cbdaPool)
})

cbdaFetch <- function(lookback) {
  # Fetch the run data from the db
  weeksBack = DBI::SQL(lookback)
  q_string <- glue::glue_sql("select s.name,
                                     r.location,
                                     r.test_software_version,
                                     r.run_start_ts,
                                     r.run_stop_ts,
                                     r.status,
                                     r.run_type,
                                     r.errors[1]::text as errors,
                                     r.warnings[1]::text as start_step,
                                     r.warnings[2]::text as stop_step,
                                     r.jenkins_url
                              from \"system\" as s,
                                   system_bring_up_run as r
                              where r.system_id = s.system_id and
                                    r.job_name = 'asic_bringup' and
                                    r.run_start_ts > now() - interval '{weeksBack} weeks'
                              order by run_stop_ts desc",
                             .con = cbdaPool,
                             weeksBack = weeksBack
                             )
    dbGetQuery(cbdaPool,
             q_string)
}

polishRunData <- function(runs) {
  runs$errors <- factor(runs$errors,
                        levels = c('START',
                                   'update_cfg_mcd',
                                   'update_cfg_power',
                                   'power_walk',
                                   'register_check_chain_break', 'lvds_bscan', 'hot_lvds_bscan', 'vddc_offset_char', 'check_atpg_si',
                                   'shmoo', 'full_die', 'health_check', 'repair_init', 'update_cfg', 'functional', 'vddc_char', 'tsens',
                                   '1200_wafer_diag_1', '1200_wafer_diag_2', '1200_wafer_diag_0', '1200_wafer_diag_5',
                                   'wafer_diag_1','wafer_diag_2', 'wafer_diag_0', 'wafer_diag_5',
                                   'over_repair',
                                   'git_commit',
                                   'stall_repair',
                                   'MISSING'
                        ))
  runs <- runs %>% rename(Step = errors)
  runs$status <- factor(runs$status,
                        levels = c('in progress', 'pass', 'fail'))
  runs$startWW <- as.factor(isoweek(runs$run_start_ts))
  runs$stopWW <- as.factor(isoweek(runs$run_stop_ts))
  runs$run_start_date <- as.Date(format(runs$run_start_ts, format = '%Y-%m-%d'))
  runs$run_stop_date <- as.Date(format(runs$run_stop_ts, format = '%Y-%m-%d'))
  runs$run_time <- runs$run_stop_ts - runs$run_start_ts
  
  runs <- runs %>% filter(grepl('.*_bringup', location))
  runs$version <- str_extract(runs$test_software_version, 'bringup_automation_sdr-([0-9.]+)', group = 1)
  
  return(runs)
}

generateYieldData <- function(runs) {
  # Compute Yields
  # run_type is the steps:start:stop steps
  # warnings[0] is in_progress or committed
  # status is in_progress during an rwb run, or pass/fail at the end of the run
  
  # get the number of starts
  yields <- runs %>% filter(start_step == "update_cfg_mcd") %>% group_by(name) %>% slice(which.max(run_start_ts)) %>% group_by(startWW) %>%
    count(name = "Starts", startWW) %>% rename('WW' = 'startWW')
  
  # compute the FPY
  yields <- yields %>% rename(stopWW = WW) %>% left_join(by = join_by(stopWW), runs %>% filter(start_step == 'update_cfg_mcd', stop_step == 'git_commit', status == 'pass') %>%
                                                           group_by(name) %>% slice(which.max(run_start_ts)) %>% group_by(stopWW) %>%
                                                           count(name = "FP", stopWW)) %>% rename('WW' = 'stopWW')
  yields <- yields %>% mutate('FPY' = FP / Starts)
  
  # compute the LPY
  yields <- yields %>% rename(stopWW = WW) %>% left_join(by = join_by(stopWW), runs %>% filter(status == 'pass', stop_step == 'git_commit') %>%
                                                           group_by(name) %>% slice(which.max(run_stop_ts)) %>% group_by(stopWW) %>%
                                                           count(name = 'LP', stopWW)) %>% rename('WW' = 'stopWW')
  yields <- yields %>% mutate('LPY' = LP / Starts)
  yields <- yields %>% mutate_at(vars(-group_cols()), list(~ replace_na(., 0)))
  #yields <- yields %>% mutate_all(list(~ replace_na(., 0)))
  
  # longer the data for plotting
  yields_long <- yields %>% select(WW, FPY, LPY) %>% gather('FPY', 'LPY', key = 'Yield', value = 'Stat')
  
  return(yields_long)
}

plotFpyFailSteps <- function(runs,
                             yieldData) {
  runs %>% filter(grepl('bringup_automation_sdr-', test_software_version)) %>%
    filter(Step != "NA") %>% group_by(name) %>% slice(which.min(run_start_ts)) %>%
    distinct(name, Step, startWW) %>%
    ggplot(aes(x = Step, fill = Step)) + geom_bar() + facet_wrap(~ startWW) + expand_limits(y = 0) +
    labs(y = 'Failures', title = 'Count of FPY Failure Step of Runs per WW', subtitle = Sys.time()) +
    theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(labels = function(x) as.integer(round(x, 0)))
}

plotFailStepsByWW <- function(runs,
                              yieldData) {
  runs %>% filter(grepl('bringup_automation_sdr-', test_software_version)) %>%
    filter(Step != "NA") %>% group_by(name) %>%
    distinct(name, Step, startWW) %>%
    ggplot(aes(x = Step, fill = Step)) + geom_bar() + facet_wrap(~ startWW) + expand_limits(y = 0) +
    labs(y = 'Failures', title = 'Count of Failure Step of Runs per WW', subtitle = Sys.time()) +
    theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), panel.grid.minor = element_blank()) +
    scale_y_continuous(labels = function(x) as.integer(round(x, 0)))
}

plotFailStepsByVersion <- function(runs,
                                   yieldData) {
  runs %>% filter(grepl('bringup_automation_sdr-', test_software_version)) %>%
    filter(Step != "NA", version != "NA") %>% group_by(name) %>%
    distinct(name, Step, version) %>%
    ggplot(aes(x = Step, fill = Step)) + geom_bar() + facet_wrap(~ version, scales = 'free_y') + expand_limits(y = 0) +
    labs(y = 'Failures', title = 'Count of Failure Step of Runs by Test Version', subtitle = Sys.time()) +
    theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), panel.grid.minor = element_blank()) +
    scale_y_continuous(labels = function(x) as.integer(round(x, 0)))
}

plotWeigthedFailStepsByVersion <- function(runs,
                                           yieldData) {
  runs %>% filter(grepl('bringup_automation_sdr-', test_software_version), Step != "NA") %>%
    count(version, Step) %>%
    group_by(version) %>%
    mutate(proportion = n / sum(n)) %>%
    ggplot(aes(x = Step, fill = Step, y = proportion)) +
    geom_col() +
    facet_wrap(~ version) +
    labs(
      title = "Weighted Distribution of Fail Steps per Test Software Version",
      subtitle = Sys.time(),
      x = "Step",
      y = "Proportion"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(
      text = element_text(size = 18),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    )
}

plotStartsPerDayByVersion <- function(runs,
                                      yieldData) {
  runs %>% filter(grepl('bringup_automation_sdr-', test_software_version)) %>% group_by(name) %>% slice(which.min(run_start_ts)) %>%
    group_by(version) %>% count(run_start_date) %>%
    ggplot(aes(x = run_start_date, y = n, group = version, color = version)) +
    geom_point() + geom_line() + expand_limits(y = 0) + scale_y_continuous(breaks = pretty_breaks()) +
    labs(y = 'Starts', title = 'Starts per Day by Version', subtitle = Sys.time()) +
    theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), panel.grid.minor = element_blank())
}

plotStartsPerWW <- function(runs,
                            yieldData) {
  runs %>% filter(grepl('bringup_automation_sdr-', test_software_version)) %>%
    group_by(name) %>% slice(which.min(run_start_ts)) %>% group_by(location) %>% count(startWW) %>%
    ggplot(aes(x = startWW, y = n, fill = location, label = n)) +
    geom_col() + expand_limits(y = 0) +
    labs(y = 'Starts', title = 'Starts per WW', subtitle = Sys.time()) +
    geom_text(position = position_stack(vjust = 0.5)) +
    theme(text = element_text(size = 18), panel.grid.minor = element_blank())
}

plotFpyLpy <- function(runs,
                       yieldData) {
  yieldData %>% ggplot(aes(x = WW, y = Stat, group = Yield, color = Yield, label = scales::percent(Stat))) +
    geom_line() + geom_point() + geom_text(nudge_y = 0.05) +
    scale_y_continuous(labels = scales::percent) + expand_limits(y = c(0, 1)) +
    labs(y = 'Percent Yield', title = 'FPY/LPY', subtitle = Sys.time()) +
    theme(text = element_text(size = 18))
}

plotStartsByWeekDay <- function(runs,
                               yieldData) {
  runs %>% filter(start_step == 'update_cfg_mcd') %>% count(run_start_date) %>% ggplot_calendar_heatmap('run_start_date', 'n') +
    scale_fill_continuous(low = 'green', high = 'red') + facet_wrap(~Year, ncol = 1) +
    labs(title = 'System starts by Weekday', subtitle = Sys.time()) +
    theme(text = element_text(size = 18))
}

plotRuntimeByVersion <- function(runs,
                                 yieldData) {
  runs %>% filter(start_step == 'update_cfg_mcd', stop_step == 'git_commit', status == 'pass', !is.na(version), run_time > 4000) %>%
    mutate(hours = run_time / 3600) %>% ggplot() + geom_histogram(bins = 60, aes(x = hours, fill = version)) +
    labs(title = 'FPY Run Time by Version', subtitle = Sys.time()) +
    theme(text = element_text(size = 18))
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = 'CS-3+ ASIC Bring-up'
  ),
  dashboardSidebar(
    radioButtons('chartSelect',
                 'Select a Chart',
                 choices = list('FPY Failure Step Counts by WW' = 1,
                                'Count of Failure Steps by WW' = 2,
                                'Count of Failure Steps by Test Version' = 3,
                                'Weighted Failure Steps by Test Version' = 4,
                                'Starts per Day by Test Version' = 5,
                                'Starts per WW by Site' = 6,
                                'FPY and LPY' = 7,
                                'System Starts by Weekday' = 8,
                                'FPY Run-time by Test Version' = 9),
                 selected = 7),
    actionButton('fetchButton',
                 'Update Data'),
    sliderInput(inputId = 'lookback',
                label = 'Weeks of data to pull',
                min = 1,
                max = 12,
                value = 5,
                step = 1),
    width = 300
  ),
  dashboardBody(
    fluidRow(
      box(width = 12, plotOutput('MainPlot'))
    )
  )
)

fetchData <- function(lookback) {
  runData <- cbdaFetch(lookback)
  runData <- polishRunData(runData)
  yieldData <- generateYieldData(runData)
  
  return(list(runData = runData,
              yieldData = yieldData))
}

fetchAndPlot <- function(plotIndex,
                         lookback,
                         output) {
  # list of plots in the same order as the UI radio buttons
  plotFuncs <- list(plotFpyFailSteps,
                    plotFailStepsByWW,
                    plotFailStepsByVersion,
                    plotWeigthedFailStepsByVersion,
                    plotStartsPerDayByVersion,
                    plotStartsPerWW,
                    plotFpyLpy,
                    plotStartsByWeekDay,
                    plotRuntimeByVersion)

  fetchedData <- fetchData(lookback)
  output$MainPlot <- renderPlot(
    plotFuncs[[as.integer(plotIndex)]](fetchedData$runData,
                                       fetchedData$yieldData),
    height = 700)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Get the initial data
  #fetchAndPlot(input$chartSelect,
  #             as.integer(input$lookback),
  #             output)
  
  trigger <- reactive({
    list(input$fetchButton,
         input$chartSelect,
         input$lookback)
  }) %>% debounce(500)
  
  observeEvent(trigger(), {
    fetchAndPlot(input$chartSelect,
                 input$lookback,
                 output)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)