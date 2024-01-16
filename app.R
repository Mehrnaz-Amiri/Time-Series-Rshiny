
# Load R packages
suppressWarnings(suppressMessages({
  library(shiny)
  library(shinyBS)
  library(shinycssloaders)
  library(shinyWidgets)
  library(shinythemes)
  library(shinyjs)
  library(htmlwidgets)
  library(readr)
  library(dplyr)
  library(reshape2)
  library(lubridate)
  library(sf)
  library(leaflet)
  library(leaflet.esri)
  library(billboarder)
  library(DT)
  library(reactable)
  library(nominatimlite)
  library(highcharter)
  library(billboarder)
  library(scales)
  library(reshape2)
  library(waiter)
  library(stringr)
  library(streamgraph)
  library(anicon)
  library(zoo)
  library(xts)
  library(ggplot2)
  library(devtools)
  library(forecast)
  library(dygraphs)
  library(stats)
  library(tsbox)
  library(tidyr)
  library(padr)
  library(rio)
  library(magrittr)
  library(rsconnect)
  
}))


source('modules/ui/action_button_css.R')
source('modules/ui/inline_block_css.R')
source('modules/ui/input_ui.R')
source('modules/ui/metric_ui.R')
source('modules/ui/chart_ui.R')
source('modules/ui/navbar_page.R')


# server modules
source('modules/server/render_timeseries.R')
source('modules/server/render_barchart.R')
source('modules/server/render_donut.R')
source('modules/server/render_table.R')
source('modules/server/render_table2.R')
source('modules/server/render_table3.R')
source('modules/server/forecast_plot.R')
source('modules/server/my_spread.R')

fp <- base64enc::dataURI(file="www/FP.png", mime="FP/png")
wmj <- read.csv('WMJ.csv',stringsAsFactors = FALSE, header = TRUE)

m <- as.POSIXct(wmj$Week.End.Date,format="%Y-%m-%d",tz=Sys.timezone())
h <- ymd(m)
wmj$Week.End.Date <- as.Date(h, format = "Y-m-d")

m <- as.POSIXct(wmj$Week.Start.Date,format="%Y-%m-%d",tz=Sys.timezone())
h <- ymd(m)
wmj$Week.Start.Date <- as.Date(h, format = "Y-m-d")

m <- as.POSIXct(wmj$INCDATE,format="%Y-%m-%d",tz=Sys.timezone())
h <- ymd(m)
wmj$INCDATE <- as.Date(h, format = "Y-m-d")

m <- as.POSIXct(wmj$INCWEEK,format="%Y-%m-%d",tz=Sys.timezone())
h <- ymd(m)
wmj$INCWEEK <- as.Date(h, format = "Y-m-d")

m <- as.POSIXct(wmj$INCMONTH,format="%Y-%m-%d",tz=Sys.timezone())
h <- ymd(m)
wmj$INCMONTH <- as.Date(h, format = "Y-m-d")

m <- as.POSIXct(wmj$INCQUAR,format="%Y-%m-%d",tz=Sys.timezone())
h <- ymd(m)
wmj$INCQUAR <- as.Date(h, format = "Y-m-d")

wmj <- wmj %>%  filter(Super.Category != "")

wmj$Client.Name <- ifelse(wmj$Client.Name == "Janssen Pharmaceuticals" | wmj$Client.Name  == "Janssen", "Janssen Pharmaceuticals", wmj$Client.Name )

wmj$Project.Type <- ifelse(wmj$Project.Type == "z_DNU - Engage - Email Content" | wmj$Project.Type == "z_DNU - Engage - Email Engagement Solutions" | wmj$Project.Type == "z_DNU -Engage - Design", "DNU-Engage", ifelse(wmj$Project.Type == "zInternal Initiative", "Internal Initiative",
                                                                                                                                                                                                                         ifelse(wmj$Project.Type == "zInternal: Gen Admin", "Internal: Gen Admin",
                                                                                                                                                                                                                                ifelse(wmj$Project.Type == "zInternal: Management", "Internal: Management",
                                                                                                                                                                                                                                       ifelse(wmj$Project.Type == "zInternal: Operations", "Internal: Operations",
                                                                                                                                                                                                                                              ifelse(wmj$Project.Type == "zInternal: PTO", "Internal: PTO",
                                                                                                                                                                                                                                                     ifelse(wmj$Project.Type == "zInternal: Training", "Internal: Training",
                                                                                                                                                                                                                                                            ifelse(wmj$Project.Type == "zNew Business", "New Business",
                                                                                                                                                                                                                                                                   ifelse(wmj$Project.Type == "zNon-Billable (Client)", "Non-Billable (Client)",
                                                                                                                                                                                                                                                                          ifelse(wmj$Project.Type == "zNon-Billable (Shift)", "Non-Billable (Shift)",wmj$Project.Type)))))))))) 
forecast_df <- read.csv('forecast_client_project.csv',stringsAsFactors = FALSE, header = TRUE)

m <- as.POSIXct(forecast_df$INCDATE,format="%Y-%m-%d",tz=Sys.timezone())
h <- ymd(m)
forecast_df$INCDATE <- as.Date(h, format = "Y-m-d")

m <- as.POSIXct(forecast_df$INCWEEK,format="%Y-%m-%d",tz=Sys.timezone())
h <- ymd(m)
forecast_df$INCWEEK <- as.Date(h, format = "Y-m-d")

m <- as.POSIXct(forecast_df$INCMONTH,format="%Y-%m-%d",tz=Sys.timezone())
h <- ymd(m)
forecast_df$INCMONTH <- as.Date(h, format = "Y-m-d")

m <- as.POSIXct(forecast_df$INCQUAR,format="%Y-%m-%d",tz=Sys.timezone())
h <- ymd(m)
forecast_df$INCQUAR <- as.Date(h, format = "Y-m-d")

#_________________________________________________________________________________________________________________
  
ui <- tagList(useShinyjs(),
              tags$style("
              body {
             -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
             zoom: 0.8; /* Other non-webkit browsers */
             zoom: 80%; /* Webkit browsers */
             }"),
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),
              
              navbarPage(radioGroupButtons('page', label = '', choices = c('Dashboard', 'Forecast', 'Project Forecast Input'),
                                            selected = c('Dashboard'), status = 'warning', individual = TRUE, size = 'lg'),
                 
                 fixedPanel(top = 0, left = 0, width = 400, height = '100%',
                            tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css')),
                            tags$head(tags$link(rel = 'stylesheet', type = 'text/css',
                                                href = 'ion.rangeSlider.skinSquare.css')),
                          wellPanel(id = 'controls', draggable = TRUE,
                                    span(h3(strong("Forecast and Staffing Analysis")),style="color:navy"),
                                    hr(),
                                    p(style="text-align: justify;","The tool will stop working if there are no results for a specific combination of choices. For example, if you want to find analytics who worked on the Pfizer project, you might use filters like 'Client Name = Pfizer Inc.' and 'Roles = Analytics.' The tool will stop working because no results were found. In such cases, please refresh the tool and try alternative options!"),
                                    hr(),
                                    p("Please select a date to view various plots for that specific date."),
                                    
                                    input_ui('date',
                                             dateRangeInput('date', 'Date range:', min = "2022-01-01", max = "2023-12-30",
                                                            start = min(wmj$Week.Start.Date, na.rm = TRUE) , end = max(wmj$Week.End.Date, na.rm = TRUE)),
                                             help = 'Analyze the data between the selected start and end dates.'
                                    ), 
                                    
                                    input_ui('agg',
                                             radioGroupButtons('agg', 'Date aggregation:',
                                                               choices = list('Day' = 'INCDATE', 'Week' = 'INCWEEK',
                                                                              'Month' = 'INCMONTH', 'Quarter' = 'INCQUAR'),
                                                               selected = list('Month' = 'INCMONTH'), status = 'info', size = 'xs',
                                                               justified = TRUE),
                                             help = 'Choose a level of aggregation to see in charts.'
                                    ),
                                    
                                    input_ui('Section',
                                             pickerInput('Section', 'FTE vs Freelancer:',
                                                         choices = levels(as.factor(wmj$Section)),
                                                         width = '100%', multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, size = 10,
                                                                                 selectOnTab = TRUE, noneSelectedText = 'Not active',
                                                                                 style = 'primary')),
                                             help = 'Filter data by FTE vs Freelancer.'
                                    ),
                                    
                                    input_ui('Client.Name',
                                             pickerInput('Client.Name', 'Client Names:',
                                                         choices = levels(as.factor(wmj$Client.Name)),
                                                         width = '100%', multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, size = 10,
                                                                                 selectOnTab = TRUE, noneSelectedText = 'Not active',
                                                                                 style = 'primary')),
                                             help = 'Filter data by one or more client names.'
                                    ),
                                    input_ui('project_types',
                                             pickerInput('project_types', 'Project Types:',
                                                         choices = levels(as.factor(wmj$Project.Type)),
                                                         width = '100%', multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, size = 10,
                                                                                 selectOnTab = TRUE, noneSelectedText = 'Not active',
                                                                                 style = 'primary')),
                                             help = 'Filter data by project types.'
                                    ),
                                    
                                    input_ui('roles',
                                             pickerInput('roles', 'Roles:',
                                                         choices = levels(as.factor(wmj$Super.Category)),
                                                         width = '100%', multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, size = 10,
                                                                                 selectOnTab = TRUE, noneSelectedText = 'Not active',
                                                                                 style = 'primary')),
                                             help = 'Filter data by one or more role types.'
                                    ),
                                    
                                   
                                    input_ui('offices',
                                             pickerInput('offices', 'Office Names:',
                                                         choices = levels(as.factor(wmj$Project.Office.Name)),
                                                         width = '100%', multiple = TRUE,
                                                         options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, size = 10,
                                                                                 selectOnTab = TRUE, noneSelectedText = 'Not active',
                                                                                 style = 'primary')),
                                             help = 'Filter data by one or more office names. Many offices may contribute to a project.'
                                    ))),
                        
                          conditionalPanel(condition = 'input.page == "Dashboard"',               
                                           div(id = 'Dashboard', div(class = 'outer',
                                                                     column(4, style = 'padding: 0; height: 50%;',
                                                                          column(12, style = 'padding: 0; height: 50%;',
                                                                                 metric_ui('Total Billing Hours', textOutput('total_billing_desc', inline = TRUE),
                                                                                           textOutput('total_billing')),
                                                                          
                                                                                metric_ui('Total PTO Hours', textOutput('total_hours_desc', inline = TRUE),
                                                                                          textOutput('total_hours'))),
                                                                          
                                                                          column(12, style = 'padding: 0; height: 50%;',
                                                                               metric_ui('Non Billable Hours', textOutput('Non.Billable.Hours_desc', inline = TRUE),
                                                                              textOutput('Non.Billable.Hours')),
                                                                     
                                                                               metric_ui('Total Non-Billable (Client) Hours', textOutput('total_hours_desc2', inline = TRUE),
                                                                                        textOutput('total_hours2')))),
                                                                     
                                                                          #chart_ui(4,'Total Billable Hours by Project Type and Offices', textOutput('ts_project_desc', inline = TRUE),
                                                                              #highchartOutput('heatmap', width = '100%')),
                                                                    
                             
                                                                     chart_ui(8, 'Total Billable Hours over Time', textOutput('ts_billable_desc', inline = TRUE), 
                                                                                   billboarderOutput('ts_billable',width = '100%')),
                                                                     
                                                                     chart_ui(4, 'Total Billable Hours by Roles', textOutput('ts_role_desc', inline = TRUE), 
                                                                              billboarderOutput('ts_role')),
                                                                     
                                                                     chart_ui(4, 'Total Billable Hours by Offices', 'Total Billable Hours by Offices Over Time',
                                                                              billboarderOutput('donut_office')),
                                                                     
                                                                     chart_ui(4, 'Billable Hours by Client and Project Names', 'Total Billable Hours by Client and Project Names', 
                                                                            DT::dataTableOutput('table_project_client'))))),
                 
                 
                 
                 
                 conditionalPanel(condition = 'input.page == "Forecast"',               
                                  div(id = 'Forecast', div(class = 'outer',
                                  
                                  chart_ui(6, 'Forecasted Billable Hours vs. Actual Billable Hours', textOutput('ts_forecast_desc', inline = TRUE), 
                                           billboarderOutput('ts_forecast',width = '100%')),
                                  chart_ui(6, 'Forecasted Billable Hours by Client, Project Names, Roles, and Offices', 'Forecasted Billable Hours vs. Actual Billable Hours', 
                                           DT::dataTableOutput('table_forecast1')),
                                  
                                  chart_ui(6, 'Box plot to show the number of billable hours required to complete each project', 'Outliers are not shown in the plot for more clarity.', 
                                           highchartOutput('boxplot')),
                                  chart_ui(6, 'Table to show the number of billable hours required to complete each project per client', '', 
                                           DT::dataTableOutput('table_stats'))))),
                 
                 
                 conditionalPanel(condition = 'input.page == "Project Forecast Input"',               
                                  div(id = 'Project Forecast Input', div(class = 'outer',
                                  fluidPage(fluidRow(
                                            tabsetPanel(type = "tabs", tags$head(
                                                                            tags$style(type='text/css', 
                                                                                       ".nav-tabs {font-size: 18px}")),
                                            tabPanel("Forecast Dataset", br(),
                                            sidebarPanel(tags$hr(),
                                                                h3("Build the dataset that we need for forecasting"),
                                                                tags$hr(),
                                                                uiOutput("client"),
                                                                conditionalPanel(
                                                                  condition = "output.client", 
                                                                  h6("Filter the dataset based on Client(s)"),
                                                                  uiOutput("project"),
                                                                  h6("Filter the dataset based on Project Type(s)"),
                                                                  checkboxInput("all", 
                                                                                label = "Select ALL/None",
                                                                                value = FALSE)),
                                                                conditionalPanel(
                                                                  condition = "output.project", 
                                                                  uiOutput("role"),
                                                                  h6("Filter the dataset based on Role Name(s)"),
                                                                  checkboxInput("all2", 
                                                                                label = "Select ALL/None", 
                                                                                value = FALSE)),
                                                                conditionalPanel(
                                                                  condition = "output.role", 
                                                                  uiOutput("office"),
                                                                  h6("Filter the dataset based on Office(s)"),
                                                                  checkboxInput("all3", 
                                                                                label = "Select ALL/None", 
                                                                                value = FALSE)),
                                                         tags$hr(),
                                                        
                                                                checkboxInput("checkbox", 
                                                                              label = "Aggregate Rows", 
                                                                              value = FALSE), # Sets the checkbox to be empty when the app initiates
                                                                tags$hr(),
                                                                # Outputs an action button that allows the user to build and display the filtered dataset when pressed
                                                                actionButton(inputId="build", "Build Dataset"),
                                                                h6("Display the dataset"),
                                                                # Assigns an id to the sidebar and the length 
                                                                id = "Sidebar Panel 1",width = 3),
                                             mainPanel(tags$hr(),
                                                       h2("Dataset"),
                                                       br(),
                                                       DT::dataTableOutput("subset_df"),id = "Main Panel 1")),
                                   tabPanel("Forecast Models",
                                            sidebarPanel(tags$hr(),
                                            h3("Generate Forecast model"),
                                            tags$hr(),selectInput(inputId="i_task_select", 
                                                                       "Select Series",
                                                                       '',
                                                                       ''),
                                                           numericInput("i_forecast_n",h5("Forecast Periods"), value = 1),
                                                           h6("Select the number of periods, between 1 and 12 months, to forecast ahead"),
                                                           br(),
                                                           # Create Select option for confidence interval
                                                           selectInput(inputId = "conf_int",
                                                                       label = "Select Confidence Interval",
                                                                       choice = c("99%" = "99", "95%" = "95","90%" = "90","80%" = "80","70%" = "70", "50%" = "50"),multiple = FALSE,width = "50%"),
                                                           br(),
                                                           # Allows the user to adjust the number of previous periods to observe how the the model would have performed
                                                           sliderInput(inputId = "i_recent_months",
                                                                       "Holdout Period",value = 3,min = 2, max = 12, step =1 ),
                                                           h6("Select the number of periods to include in the Holdout Period"),
                                                           tags$hr(),
                                                           actionButton(inputId="goButton", "Start forecasting!"), width = 3,
                                                           h6("Click on the tabs at the top right to switch between models")),
                                          
                                                  mainPanel(tabsetPanel(type = "tabs", 
                                                                         tags$head(
                                                                           tags$style(type='text/css', 
                                                                                      ".nav-tabs {font-size: 18px} ")),
                                                
                                                            tabsetPanel(type = "tabs",
                                                                                 # Build forecasting panels
                                                                                 tabPanel("ARIMA", icon = icon("area-chart"), h4("Auto Regressive Integrated Moving Average (ARIMA) Model"), br(), dygraphOutput("p_ARIMA"), value=1),
                                                                                 tabPanel("ETS", icon = icon("line-chart"), h4("Error Trend Seasonal Model"), br(), dygraphOutput("p_ets"), value=2),
                                                                                 tabPanel("MA", icon = icon("line-chart"), h4("Moving Average Smoothing Model"), br(), dygraphOutput("p_MA"), value=3),
                                                                                 tabPanel("SES", icon = icon("line-chart"), h4("Simple Exponential Smoothing Model"), br(), dygraphOutput("p_SES"), value=4),
                                                                                 id = "timeSeriesTabs"),
                                                                     id = "Tab Panel"),
                                                            
                                                            
                                                            fluidRow(
                                                              column(width=9,
                                                                     h3("Model Performance for Holdout Period"),
                                                                     hr(),
                                                                     h5("What is a good mean absolute percentage error? A MAPE less than 5% is considered as an indication that the forecast is acceptably accurate. A MAPE greater than 10% but less than 25% indicates low, but acceptable accuracy and MAPE greater than 25% very low accuracy."),
                                                                     hr(),
                                                                     DT::dataTableOutput("model_performance"))))),
                                   
                                   tabPanel("Forecast Results",
                                     fluidPage(fluidRow(sidebarPanel(
                                           tags$hr(),
                                           h3("Generate Batch Forecasts"),
                                           tags$hr(),
                                           h4("Adjust Data:"),
                                           h6("Select the number of maximum zero observations within the recent six periods"),
                                           selectInput(inputId = "zero_obs",
                                                       label = "",
                                                       choice = c("6" = "6", 
                                                                  "5" = "5",
                                                                  "4" = "4",
                                                                  "3" = "3",
                                                                  "2" = "2"),
                                                       multiple = FALSE,
                                                       width = "50%"),
                                           tags$hr(),
                                               h5("Select Model(s)"),
                                               checkboxInput("ARIMAmodel",
                                                             label = "ARIMA model",
                                                             value = TRUE),
                                               checkboxInput("ETSmodel",
                                                             label = "ETS model",
                                                             value = FALSE),
                                              
                                               checkboxInput("MAmodel",
                                                             label = "MA model",
                                                             value = FALSE),
                                               checkboxInput("SESmodel",
                                                             label = "SES model",
                                                             value = FALSE),
                                               tags$hr(),
                                               # Allows the user to adjust how may periods ahead to forecast ahead
                                              numericInput("forecast_n",h5("Forecast Periods"), value = 1),
                                               h6("Select the number of periods to forecast ahead"),
                                               tags$hr(),
                                               selectInput(inputId = "conf_int2",
                                                           label = "Select Confidence Interval",
                                                           choice = c("99%" = "99",
                                                                      "95%" = "95",
                                                                      "90%" = "90",
                                                                      "80%" = "80",
                                                                      "70%" = "70",
                                                                      "50%" = "50"),
                                                           multiple = FALSE,
                                                           width = "50%"),
                                               tags$hr(),
                                               # Allows the user to adjust the number of previous periods to observe how the the model would have performed
                                               sliderInput(inputId = "i_recent_months2",
                                                           "Holdout Period",
                                                           value = 3,
                                                           min = 2, max = 12, step = 1),
                                               h6("Select the number of periods to include in the Holdout Period"),
                                               tags$hr(),
                                               selectInput(inputId = "error_measurement",
                                                           label = "Select Error Measurement",
                                                           choice = c("RMSE",
                                                                      "MAE",
                                                                      "MAPE"),
                                                           multiple = FALSE,
                                                           width = "50%"),
                                               # Outputs an action button that allows the user to begin forecasting 
                                               actionButton(inputId="batch_FC", "Start Batch forecasting!")
                                             ,id = "Sidebar Panel 2", width = 3),
                                         mainPanel(
                                           tags$hr(),
                                           tabPanel("Forecast Data Table",
                                                    tabsetPanel(type = "tabs",
                                                                # Build forecasting panels
                                                                tabPanel("Batch Forecast", icon = icon("area-chart"), br(), h4("To see the results for Batch Forecast, your forecast dataset must contain at least two columns other than Date, such as two selected client names with one project type or two project types with one client name.",style="color:brown"), hr(), DT::dataTableOutput("final_batchdf")),
                                                                tabPanel("Recommended Forecast", icon = icon("area-chart"), br(), h4("Recommended Forecast for Billable Hours based on Selected Error Measurement"), br(), DT::dataTableOutput("model_recommend_df")),
                                                                
                                                                id = "timeSeriesTabs"),
                                                    id = "Tab Panel"),
                                           id = "Main Panel 2")))),
                                   
                                   
                                   tabPanel("Linear Regression Forecast", hr(),
                                                     h3("Predict billable hours for the next three months"), 
                                                     br(),
                                                     h4('Please select client name, project type, and department, then choose between months 10, 11, or 12.'),
                                                      selectInput(inputId = "reg_months",
                                                        label = "",
                                                        choice = c("10" = "10", 
                                                                   "11" = "11",
                                                                   "12" = "12"),
                                                        multiple = FALSE,
                                                        width = "50%"),
                                                     column(6, style = 'padding: 0; height: 50%;',
                                                            column(12, style = 'padding: 0; height: 100%;',
                                                                   metric_ui('Forecast Billable Hours', textOutput('total_billing_regression', inline = TRUE),
                                                                             textOutput('billing_regression')))),
                                                     br(), hr(), DT::dataTableOutput("billing_regression_table"))))))))))
              
              
         
                 
                 
                 
                


               
               
#-----------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  collisions_js =  JS(read_file('www/collisions.js'))
  
  # animate icons for collapsible layer panels
  runjs("$('.panel-collapse').on('show.bs.collapse', function () {
               $(this).siblings('.panel-heading').addClass('active');
           });
           $('.panel-collapse').on('hide.bs.collapse', function () {
               $(this).siblings('.panel-heading').removeClass('active');
           });")


  observe({

    # filter
    my_wmj = wmj %>% filter(INCDATE >= as.Date(input$date[1]), INCDATE <= as.Date(input$date[2]))
    if(length(input$roles) > 0) {
      my_wmj = my_wmj %>% filter(Super.Category %in% input$roles)
    }
    if(length(input$offices) > 0) {
      my_wmj = my_wmj %>% filter(Project.Office.Name %in% input$offices)
    }
    if(length(input$project_types) > 0) {
      my_wmj = my_wmj %>% filter(Project.Type %in% input$project_types)
    }
    if(length(input$Client.Name) > 0) {
      my_wmj = my_wmj %>% filter(Client.Name %in% input$Client.Name)
    }
    if(length(input$Section) > 0) {
      my_wmj = my_wmj %>% filter(Section %in% input$Section)
    }
    
    
    # 
    by_date = my_wmj %>% group_by(!!sym(input$agg)) %>% summarise(`Billable Hours` = sum(Billable.Hours, na.rm = TRUE))
    
    by_date2 = my_wmj %>% group_by(!!sym(input$agg)) %>% summarise(Non.Billable.Hours = sum(Actual.Non.Billable.Hours, na.rm = TRUE))
    
    by_date3 = my_wmj %>% filter(Project.Type.New == 'PTO') %>% group_by(!!sym(input$agg)) %>% summarise(Actual.Hours.Worked = sum(Actual.Hours.Worked, na.rm = TRUE))
    
    by_date4 = my_wmj %>% filter(Project.Type.New == 'Non-Billable (Client)') %>% group_by(!!sym(input$agg)) %>% summarise(Actual.Hours.Worked = sum(Actual.Hours.Worked, na.rm = TRUE))
    
    by_mode = my_wmj %>% group_by(!!sym(input$agg), Project.Type) %>% summarise(Billable.Hours = sum(Billable.Hours, na.rm = TRUE))
    by_mode = by_mode %>% group_by(Project.Type) %>% summarise(Billable.Hours = round(sum(Billable.Hours, na.rm = TRUE)))
    
    by_roles = my_wmj %>% group_by(!!sym(input$agg), Super.Category) %>% summarise(Billable.Hours = sum(Billable.Hours, na.rm = TRUE))
    by_roles = by_roles %>% group_by(Super.Category) %>% summarise(Billable.Hours = round(sum(Billable.Hours, na.rm = TRUE)))
    
    
    by_offices = my_wmj %>% group_by(!!sym(input$agg), Project.Office.Name) %>% summarise(Billable.Hours = sum(Billable.Hours, na.rm = TRUE))
    by_offices = by_offices %>% group_by(Project.Office.Name) %>% summarise(Billable.Hours = round(sum(Billable.Hours, na.rm = TRUE)))
    
    by_client = my_wmj %>% group_by(!!sym(input$agg), Client.Name) %>% summarise(Billable.Hours = sum(Billable.Hours, na.rm = TRUE))
    by_client = by_client %>% group_by(Client.Name) %>% summarise(Billable.Hours = round(sum(Billable.Hours, na.rm = TRUE)))
    
    by_section = my_wmj %>% group_by(!!sym(input$agg), Section) %>% summarise(Billable.Hours = sum(Billable.Hours, na.rm = TRUE))
    by_section = by_section %>% group_by(Section) %>% summarise(Billable.Hours = round(sum(Billable.Hours, na.rm = TRUE)))
    
    by_client2 = my_wmj %>% group_by(Client.Name,Project.Name) %>% summarise(Billable.Hours = sum(Billable.Hours, na.rm = TRUE))
    
    # metrics
    output$total_billing = renderText({
      round(sum(by_date$`Billable Hours`, na.rm = TRUE))
    })
    output$total_billing_desc = renderText({
      paste0('Total Billing Hours Between ', input$date[1], ' and ', input$date[2])
    })
   
    output$Non.Billable.Hours = renderText({
      round(sum(by_date2$Non.Billable.Hours, na.rm = TRUE))
    })
    output$Non.Billable.Hours_desc = renderText({
      paste0('Total Non-Billable Hours Between ', input$date[1], ' and ', input$date[2])
    })
    
    output$total_hours = renderText({
      round(sum(by_date3$Actual.Hours.Worked, na.rm = TRUE))
    })
    output$total_hours_desc = renderText({
      paste0('Total PTO Hours Between ', input$date[1], ' and ', input$date[2])
    })
    
    output$total_hours2 = renderText({
      round(sum(by_date4$Actual.Hours.Worked, na.rm = TRUE))
    })
    
    output$total_hours_desc2 = renderText({
      paste0('Total Non-Billable (Client) Hours Between ', input$date[1], ' and ', input$date[2])
    })
    
    # charts
    output$ts_billable = render_timeseries(by_date)
    
    agg_name = switch(input$agg, 'INCDATE' = 'day', 'INCWEEK' = 'week', 'INCMONTH' = 'month', 'INCQUAR' = 'quarter')
    
    output$ts_Billable_desc = renderText({
      paste0('Total Billable Hours by ', agg_name)
    })
    
    
   by_roles1 = my_wmj %>% group_by(!!sym(input$agg), Super.Category) %>% summarise(`Billable Hours` = round(sum(Billable.Hours, na.rm = TRUE)),2)
   j <- reshape2::dcast(by_roles1, get(input$agg) ~Super.Category, value.var = 'Billable Hours')
   j <- as_tibble(j)
   output$ts_role= render_barchart(j)
    
    output$ts_role_desc = renderText({
      paste0('Total Billable Hours by Roles by ', agg_name)
    })
    
    by_offices <- by_offices %>%  filter(Billable.Hours > 0)
    output$donut_office = render_donut(by_offices)
    

    
    output$table_project_client = render_table(by_client2)
    
    output$ts_project_desc = renderText({
    paste0('Billable Hours by Project Types by ', agg_name)

    })
    

####################################### Forecast ####################################
    
    forecast_df = forecast_df %>% filter(INCDATE >= as.Date(input$date[1]), INCDATE <= as.Date(input$date[2]))

    if(length(input$project_types) > 0) {
      forecast_df = forecast_df %>% filter(Project.Type %in% input$project_types)
    }
    if(length(input$Client.Name) > 0) {
      forecast_df = forecast_df %>% filter(Client.Name %in% input$Client.Name)
    }
    if(length(input$Section) > 0) {
      forecast_df = forecast_df %>% filter(Section %in% input$Section)
    }
    if(length(input$roles) > 0) {
      forecast_df = forecast_df %>% filter(Super.Category %in% input$roles)
    }
    if(length(input$offices) > 0) {
      forecast_df = forecast_df %>% filter(Project.Office.Name %in% input$offices)
    }
    
    
    dd <- forecast_df %>%  group_by(!!sym(input$agg)) %>%  summarise(`Actual Billable Hours` = sum(Actual, na.rm = TRUE),
                                                            `Forecasted Billable Hours` = sum(Forecast_TimeSeries, na.rm = TRUE))
    output$ts_forecast <- forecast_plot(dd)

    
    output$ts_forecast_desc = renderText({
      paste0('Forecasted Billable Hours by ', agg_name)
    })
    
    

    forcast_table <- forecast_df %>%  group_by(Client.Name,Project.Name,Project.Type,Super.Category,Project.Office.Name) %>%  summarise(`Actual Hours` = sum(Actual, na.rm = TRUE),
                                                                                                                                                         `Forecasted Hours` = sum(Forecast_TimeSeries, na.rm = TRUE),
                                                                                                                                                         Error = round(abs(`Forecasted Hours` - `Actual Hours`)))
    output$table_forecast1 = render_table(forcast_table)
    
    y = my_wmj %>% mutate(INCMONTH =format(as.Date(INCMONTH), '%b'), Weeks = week(INCDATE), Days = day(INCDATE)) %>% 
    group_by(Days, Weeks, INCMONTH, Project.Type) %>% summarise(Billable.Hours = sum(Billable.Hours, na.rm = TRUE))
    
    output$boxplot <- renderHighchart({
      hcboxplot(x = y$Billable.Hours, var = y$Project.Type, outliers = F, name = 'Boxplot', color = '#669900') 
    })
    

    stat <- my_wmj %>% group_by(Client.Name, Project.Type) %>% summarise(`Total Billable Hours` = round(sum(Billable.Hours, na.rm = TRUE),1),
                                                                       `Averag Billable Hours` = round(mean(Billable.Hours, na.rm = TRUE),1),
                                                                       `Median Billable Hours` = round(median(Billable.Hours, na.rm = TRUE),1),
                                                                       `Minimum Billable Hours` = round(min(Billable.Hours , na.rm = TRUE),1),
                                                                       `Maximum Billable Hours` = round(max(Billable.Hours, na.rm = TRUE),1))
    
    
  output$table_stats = render_table2(stat)
  
  
  ############################################################ FORECASTING MODELS ##############################################################
  mySeries_raw <- reactive({
    df <- my_wmj
    df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
    return(df)
    
  })
  output$client <- renderUI({
    data <- mySeries_raw()
    
    if(is.null(data)){return(NULL)}
    
    selectInput(inputId = "client",
                label = "Select Client",
                choice = sort(unique(data$Client.Name)),
                multiple = TRUE)
  })
  
  # Filter the raw data based on regions selected
  client_df <- reactive({
    data <- mySeries_raw()
    
    if(is.null(data)){return(NULL)}
    
    data %>% 
      filter(Client.Name %in% input$client)
  })
  
  # Create select option for all markets available in the regions selected in previous filter
  output$project <- renderUI({
    data <- client_df()
    
    if(is.null(data)){return(NULL)}
    
    selectInput(inputId = "project",
                label = "Select Project Type",
                choice = sort(unique(data$Project.Type)),
                multiple = TRUE)
  })
  
  # Create a checkbox option to include all markets in filter 
  observe({
    data <- client_df()
    updateSelectInput(
      session, 
      "project", 
      choices = sort(unique(data$Project.Type)),
      selected = if(input$all) unique(data$Project.Type)
    )
  })
  
  # Filter the previous dataset of selected regions based on markets selected
  project_df <- reactive({
    data <- client_df()
    
    if(is.null(data)){return(NULL)}
    
    data %>% 
      filter(Project.Type %in% input$project)
  })
  
  # Create select option for all products available in the markets selected in previous filter
  output$role <- renderUI({
    data <- project_df()
    
    if(is.null(data)){return(NULL)}
    
    selectInput(inputId = "role",
                label = "Select Role",
                choice = sort(unique(data$Super.Category)),
                multiple = TRUE)
  })
  
  # Create a checkbox option to include all products in filter
  observe({
    data <- project_df()
    updateSelectInput(
      session, 
      "role", 
      choices = sort(unique(data$Super.Category)),
      selected = if(input$all2) unique(data$Super.Category)
    )
  })
  
  # Filter the previous dataset of selected markets based on products selected
  role_df <- reactive({
    data <- project_df()
    
    if(is.null(data)){return(NULL)}
    
    data %>% 
      filter(Super.Category %in% input$role)
  })
  
  # Create select options for all SKUs in the products selected in previous filter
  output$office <- renderUI({
    data <- role_df()
    
    if(is.null(data)){return(NULL)}
    
    selectInput(inputId = "office",
                label = "Select Office",
                choice = sort(unique(data$Project.Office.Name)),
                multiple = TRUE)
  })
  
  # Create a checkbox option to include all SKUs in filter
  observe({
    data <- role_df()
    updateSelectInput(
      session, 
      "office", 
      choices = sort(unique(data$Project.Office.Name)),
      selected = if(input$all3) unique(data$Project.Office.Name)
    )
  })
  
  # Filter the previous dataset of selected products based on SKUs chosen and build the dataframe based on the action button "Build Dataset"
  final_df <- eventReactive(input$build, {
    data <- role_df()
    
    if(is.null(data)){return(NULL)}
    
    data <- data[, -which(names(data) %in% c("Super.Category"))]
    
    subset_data <- data %>% 
      filter(Project.Office.Name %in% input$office)
    
    subset_data <- subset_data %>% 
      my.spread(key = c("Client.Name", "Project.Type", "Project.Office.Name"), value = c("Billable.Hours")) %>% 
      pad(interval = "month")
    
    colnames(subset_data) <- make.names(colnames(subset_data))
    # Replace the filled in 99999.99 values with NA
    subset_data[subset_data == 99999.99] <- 0

    
    
    if (input$checkbox) {
      if(ncol(subset_data) < 3) {
        return(subset_data)
      } else {
        subset_data$Row_Total <- rowSums(subset_data[,-1], na.rm = TRUE)
      }
    }
    
    return(subset_data)
  })
  
  # Render the final filtered dataset
  output$subset_df <- renderDataTable(extension = c("FixedColumns", "Scroller"), options = list(deferRender = TRUE,
                                                                                                scrollX = TRUE, 
                                                                                                scrollY = 1000,
                                                                                                scroller = TRUE,
                                                                                                fixedColumns = list(leftColumns = 2)),{
                                                                                                  final_df() 
                                                                                                })
  

 

  observeEvent(final_df(), {
    
    mySeries <- final_df()
    
    updateSelectInput(session,
                      'i_task_select',
                      label = 'Select Series',
                      choices = names(select(mySeries, -INCMONTH)),
                      names(select(mySeries, -INCMONTH))[1])
    
    #REMOVEUPDATE
    updateSelectInput(session,
                      'i_task_select2',
                      label = 'Select Series',
                      choices = names(select(mySeries, -INCMONTH)),
                      names(select(mySeries, -INCMONTH))[1])
  })
  
  mySeries_filtered <- eventReactive(input$goButton, {
    
    if (nrow(final_df())==0) 
      return()
    
    # Reset predictions for model performance
    prediction_arima <- 0 # Set the vector 0 to store predicted values
    
    prediction_ets <- 0 # Set the vector 0 to store predicted values
    
    prediction_hw_A <- 0 # Set the vector 0 to store predicted values
    
    prediction_ma <- 0 # Set the vector 0 to store predicted values
    
    prediction_ses <- 0 # Set the vector 0 to store predicted values
    
    
    j <- 1 # Sets the first index to 1 to store predicted values
    m <- 1 # Sets the first index to 1 to store predicted values
    p <- 1 # Sets the first index to 1 to store predicted values
    s <- 1 # Sets the first index to 1 to store predicted values
    t <- 1 # Sets the first index to 1 to store predicted values
   
    # Use existing reactive structures
    mySeries <- as.data.frame(final_df())
    
    isolate({
      if(input$i_task_select ==""){
        task_type = select_(mySeries,
                            .dots = list(quote(-INCMONTH)))
        task_type = names(task_type[1])
      } else
      {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      recent_months <- input$i_recent_months
    })
    
    # Build Dataframe
    mySeries_filtered <- mySeries %>% 
      select_(.dots = list(quote(INCMONTH), 
                           task_type)) 
  })  
  
  #REMOVEUPDATE
  mySeries_filtered2 <- eventReactive(input$stat_button, {
    
    #### Input$stat_button ####
    if (nrow(final_df())==0) 
      return()
    
    # Use existing reactive structures
    mySeries <- as.data.frame(final_df())
    
    isolate({
      if(input$i_task_select2 ==""){
        task_type = select_(mySeries,
                            .dots = list(quote(-INCMONTH)))
        task_type = names(task_type[1])
      } else
      {
        task_type = input$i_task_select2
      }
    })
    
    #REMOVEUPDATE
    mySeries_filtered2 <- mySeries %>% 
      select_(.dots = list(quote(INCMONTH), 
                           task_type))
  })
  
  ####################################
  ####### AUTO ARIMA DYGRAPH  #######
  ###################################
  
  output$p_ARIMA <- renderDygraph({
    
    # Use existing reactive structures
    mySeries <- final_df()
    mySeries_ARIMA <- mySeries_filtered()
    
    isolate({
      mySeries_ARIMA <- mySeries_ARIMA %>%
        filter(INCMONTH >= as.Date(input$date[1]) &
                 INCMONTH <= as.Date(input$date[2]))
    })
    
    
    if (nrow(mySeries_ARIMA) == 0){
      stop(
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'))
      )
    }
    
    # Make inputs dependent on users hitting 'start forecasting' button
    isolate({
      if(input$i_task_select ==""){
        task_type = select_(mySeries,
                            .dots = list(quote(-INCMONTH)))
        task_type = names(task_type[1])
      } else {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      recent_months <- input$i_recent_months
    })
    
    # Convert to TS object with monthly frequency
    myY <-  xts(select(mySeries_ARIMA,
                        task_type),
                order.by=ymd(mySeries_ARIMA$INCMONTH))
    
    # Set the start of the TS object until at the start of the first numeric observation
    for(i in 1:nrow(myY)){
      if(is.na(myY[i])){
        next
      } else{
        myY <- myY[i:nrow(myY)]
        break
      }
    }
    
  
    
    withProgress(message = 'Generating Graph...  ',
                 detail = 'This may take a few seconds',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   # Forecast n periods using model with 50% and 80% confidence intervals
                   if(nrow(myY) < 2){
                     return(NULL)
                   } else{
                     isolate({
                       TS_mySeries_ARIMA <- forecast(auto.arima(myY,
                                                                stepwise = FALSE,
                                                                approximation = TRUE),
                                                     h = forecast_n,
                                                     level = c(as.numeric(input$conf_int)))
                     })
                     
                     # Convert elements of time series FORECAST to dataframe for plotting
                     forecast_ARIMA_df <- with(TS_mySeries_ARIMA,
                                               data.frame(Mean=TS_mySeries_ARIMA$mean,
                                                          Upper=TS_mySeries_ARIMA$upper[,1],
                                                          Lower=TS_mySeries_ARIMA$lower[,1]))
                     # Add Date column to the forecasted values data.frame
                     forecast_ARIMA_df$Date <- seq(as.Date(max(mySeries_ARIMA$INCMONTH)) %m+% months(1),
                                                   by = "month",
                                                   length.out = forecast_n)
                     
                     
                     forecast_ARIMA_df <- forecast_ARIMA_df %>%
                       select(Date, Mean, Upper, Lower) %>%
                       mutate(Mean = ifelse(as.integer(Mean) < 0,
                                            0,
                                            as.integer(round(Mean, 2))),
                              Upper = ifelse(as.integer(Upper) < 0,
                                             0,
                                             as.integer(round(Upper, 2))),
                              Lower = ifelse(as.integer(Lower) < 0,
                                             0,
                                             as.integer(round(Lower, 2))))
                     
                     # Convert xts object to ts object
                     y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                     m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                     y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                     m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                     
                     tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                     #tsmyY2 <- replace(tsmyY, tsmyY == 0, 1) # Replaces 0s in the ts with .01 to calculate MAPE later on
                     
                     # Obtain the start and end date of the time series in the form of a ratio
                     timeProp <- tsp(tsmyY)[1] # Takes the first observation and creates a numerical representation of the date
                     timeProp2 <- tsp(tsmyY)[2] # Takes the last observation and creates a numerical representation of the date
                     # Create start and end point of holdout period
                     holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                     holdout_end <- nrow(myY)-forecast_n
                     prediction <- 0 # Set the vector 0 to store predicted values
                     j <- 1 # Sets the first index to 1 to store predicted values
                     
                     for (i in holdout_start:holdout_end){
                       
                       if(i < 2){
                         prediction[j] <- NA
                         j <- j+1
                       } else{
                         train <- window(tsmyY, end = timeProp + ((i-1)/12)) # Creates the first training dataset to use for forecasting
                         
                         if(nmonths(train) < 3){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           FC_arima <- forecast(auto.arima(train, stepwise = FALSE, approximation = TRUE) , h = forecast_n) # Creates the model and forecasts
                           prediction[j] <- FC_arima[[4]][[forecast_n]] #Store the predicted forecast in the vector prediction with index j
                           j <- j+1 # Creates another index to store the next prediction
                         }
                       }
                     }
                     # Replace negative predictions with 0
                     prediction <- replace(prediction, prediction < 0, 0)
                     arima_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12) # Converts the predictions into a TS object for plotting and measureing accuracy
                     
                     
                     ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                     myX <- xts(select(mySeries_ARIMA, series = task_type),
                                select(mySeries_ARIMA, quote(INCMONTH)),
                                order.by=ymd(mySeries_ARIMA$INCMONTH))
                     
                     # Converts prediction into xts object for plotting
                     myPred <- xts(select(forecast_ARIMA_df,
                                           quote(Mean),
                                           quote(Upper),
                                           quote(Lower)),
                                   order.by = ymd(forecast_ARIMA_df$Date))
                     
                     # Converts previous forecasts from ts object to xts object
                     xts_arima <- xts(arima_FC,
                                      order.by = as.Date(as.yearmon(time(arima_FC))))
                     
                     myDy <- cbind(myX,
                                   myPred,
                                   xts_arima)
                     
                     
                     # Plots the dygraph
                     d <- dygraph(myDy[,1:5], main=paste0('ARIMA FORECAST OF: ', task_type, ' for ', forecast_n, ' Periods' )) %>%
                       dyAxis("x", drawGrid = FALSE) %>%
                       #dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                       dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                       dySeries('xts_arima', label = "past predictions") %>%
                       dySeries('series') %>%
                       dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                       dyLegend(width = 400) %>%
                       dyRangeSelector()
                     
                     print(d)
                   }
                 })
  })
  
  ######################################
  ##### Error Trend Seasonal Model #####
  ######################################
  
  output$p_ets <- renderDygraph({
    
    # Use existing reactive structures
    mySeries <- final_df()
    mySeries_ets <- mySeries_filtered()
    
    isolate({
      mySeries_ets <- mySeries_ets %>%
        filter(INCMONTH >= as.Date(input$date[1]) &
                 INCMONTH <= as.Date(input$date[2]))
    })
    
    if (nrow(mySeries_ets) == 0){
      stop(
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'))
      )
    }
    
    #make inputs dependent on users hitting 'start forecasting' button
    isolate({
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-INCMONTH)))
        task_type = names(task_type[1])
      } else {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      recent_months <- input$i_recent_months
    })
    
    # Convert to TS object with monthly frequency
    myY <-  xts(select_(mySeries_ets, task_type),
                order.by=ymd(mySeries_ets$INCMONTH))
    
    # Set the start of the TS object until at the start of the first numeric observation
    for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
      if(is.na(myY[i])){
        next
      } else{
        myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
        break # Breaks the for loop once the first numeric observation is found
      }
    }
    
  
    
    withProgress(message = 'Generating Graph...  ',
                 detail = 'This may take a few seconds',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   if(nrow(myY) < 3){
                     return(NULL)
                   } else{
                     # Finds the best ets model
                     TS_mySeries_ets <- ets(myY,
                                            allow.multiplicative.trend = TRUE,
                                            opt.crit = c("lik", "amse", "mse", "sigma", "mae"))
                     
                     # Forecast n periods using model
                     isolate({
                       forecast_ets <- forecast(TS_mySeries_ets,
                                                h=forecast_n,
                                                level = c(as.numeric(input$conf_int)))
                     })
                     
                     # Convert elements of time series FORECAST to dataframe for plotting
                     forecast_ets_df <- with(forecast_ets,
                                             data.frame(Mean=forecast_ets$mean,
                                                        Upper=forecast_ets$upper[,1],
                                                        Lower=forecast_ets$lower[,1]))
                     
                     forecast_ets_df$Date <- seq(as.Date(max(mySeries_ets$INCMONTH)) %m+% months(1),
                                                 by = "month",
                                                 length.out = forecast_n)
                     
                     forecast_ets_df <- forecast_ets_df %>%
                       select(Date, Mean, Upper, Lower) %>%
                       mutate(Mean = ifelse(as.integer(Mean) < 0,
                                            0,
                                            as.integer(round(Mean, 2))),
                              Upper = ifelse(as.integer(Upper) < 0,
                                             0,
                                             as.integer(round(Upper, 2))),
                              Lower = ifelse(as.integer(Lower) < 0,
                                             0,
                                             as.integer(round(Lower, 2))))
                     
                     y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                     m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                     y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                     m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                     tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                     
                     # Obtain the start and end date of the time series in the form of a ratio
                     timeProp <- tsp(tsmyY)[1]
                     timeProp2 <- tsp(tsmyY)[2]
                     # Create start and end point of holdout period
                     holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                     holdout_end <- nrow(myY)-forecast_n
                     prediction <- 0
                     j <- 1
                     
                     for (i in holdout_start:holdout_end){
                       
                       if(i < 2){
                         prediction[j] <- NA
                         j <- j+1
                       } else{
                         train <- window(tsmyY, end = timeProp + ((i-1)/12))
                         
                         if(nmonths(train) < 3){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           FC_ets <- forecast(ets(train,
                                                  allow.multiplicative.trend = TRUE,
                                                  opt.crit = c("lik", "amse", "mse", "sigma", "mae")),
                                              h = forecast_n)
                           prediction[j] <- FC_ets[[2]][[forecast_n]]
                           j <- j+1
                         }
                       }
                     }
                     
                     # Replace negative predictions with 0
                     prediction <- replace(prediction, prediction < 0, 0)
                     ets_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                     
                     ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                     myX <- xts(select_(mySeries_ets, series = task_type),
                                select_(mySeries_ets, quote(INCMONTH)),
                                order.by=ymd(mySeries_ets$INCMONTH))
                     
                     
                     myPred <- xts(select_(forecast_ets_df,
                                           quote(Mean),
                                           quote(Upper),
                                           quote(Lower)),
                                   order.by = ymd(forecast_ets_df$Date))
                     
                     xts_ets <- xts(ets_FC,
                                    order.by = as.Date(as.yearmon(time(ets_FC))))
                     
                     myDy <- cbind(myX, myPred, xts_ets)
                     
                     d <- dygraph(myDy[,1:5], main=paste0('ETS FORECAST of: ', task_type, ' for ', forecast_n, ' periods' )) %>%
                       dyAxis("x", drawGrid = FALSE) %>%
                       dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                       dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                       dySeries('xts_ets', label = "past predictions") %>%
                       dySeries('series') %>%
                       dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                       dyLegend(width = 400) %>%
                       dyRangeSelector()
                     
                     print(d)
                   }
                 })
    
  })
 
  
  ################################
  ##### Moving Average Model #####
  ################################
  
  
  output$p_MA <- renderDygraph({
    
    # Use existing reactive structures
    mySeries <- final_df()
    mySeries_MA <- mySeries_filtered()
    
    isolate({
      mySeries_MA <- mySeries_MA %>%
        filter(INCMONTH >= as.Date(input$date[1]) &
                 INCMONTH <= as.Date(input$date[2]))
    })
    
    if (nrow(mySeries_MA) == 0){
      stop(
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'))
      )
    }
    
    # Make inputs dependent on users hitting 'start forecasting' button
    isolate({
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-INCMONTH)))
        task_type = names(task_type[1])
      } else {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      recent_months <- input$i_recent_months
    })
    
    # Convert to TS object with monthly frequency
    myY <-  xts(select_(mySeries_MA, task_type),
                order.by=ymd(mySeries_MA$INCMONTH))
    
    # Set the start of the TS object until at the start of the first numeric observation
    for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
      if(is.na(myY[i])){
        next
      } else{
        myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
        break # Breaks the for loop once the first numeric observation is found
      }
    }
    

    
    withProgress(message = 'Generating Graph... ',
                 detail = 'This may take a few seconds',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   if(nrow(myY) < 3){
                     return(NULL)
                   } else{
                     # Define moving-average model
                     TS_mySeries_MA <- auto.arima(myY, max.p=0, stationary=TRUE, seasonal=FALSE)
                     
                     # Forecast n periods using model
                     isolate({
                       forecast_MA <- forecast(TS_mySeries_MA,
                                               h=forecast_n,
                                               level = c(as.numeric(input$conf_int)))
                     })
                     
                     
                     # Convert elements of time series FORECAST to dataframe for plotting
                     forecast_MA_df <- with(forecast_MA,
                                            data.frame(Mean=forecast_MA$mean,
                                                       Upper=forecast_MA$upper[,1],
                                                       Lower=forecast_MA$lower[,1]))
                     
                     forecast_MA_df$Date <- seq(as.Date(max(mySeries_MA$INCMONTH)) %m+% months(1),
                                                by = "month",
                                                length.out = forecast_n)
                     
                     forecast_MA_df <- forecast_MA_df %>%
                       select(Date, Mean, Upper, Lower) %>%
                       mutate(Mean = ifelse(as.integer(Mean) < 0,
                                            0,
                                            as.integer(round(Mean, 2))),
                              Upper = ifelse(as.integer(Upper) < 0,
                                             0,
                                             as.integer(round(Upper, 2))),
                              Lower = ifelse(as.integer(Lower) < 0,
                                             0,
                                             as.integer(round(Lower, 2))))
                     
                     y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                     m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                     y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                     m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                     tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                     
                     # Obtain the start and end date of the time series in the form of a ratio
                     timeProp <- tsp(tsmyY)[1]
                     timeProp2 <- tsp(tsmyY)[2]
                     # Create start and end point of holdout period
                     holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                     holdout_end <- nrow(myY)-forecast_n
                     prediction <- 0
                     j <- 1
                     
                     for (i in holdout_start:holdout_end){
                       
                       if(i < 2){
                         prediction[j] <- NA
                         j <- j+1
                       } else{
                         train <- window(tsmyY, end = timeProp + ((i-1)/12))
                         
                         if(nmonths(train) < 3){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           FC_ma <- forecast(auto.arima(train,
                                                        max.p=0,
                                                        stationary=TRUE,
                                                        seasonal=FALSE),
                                             h = forecast_n)
                           prediction[j] <- FC_ma[[4]][[forecast_n]]
                           j <- j+1
                         }
                       }
                     }
                     
                     # Replace negative predictions with 0
                     prediction <- replace(prediction, prediction < 0, 0)
                     ma_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                     
                     ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                     myX <- xts(select_(mySeries_MA, series = task_type),
                                select_(mySeries_MA, quote(INCMONTH)),
                                order.by=ymd(mySeries_MA$INCMONTH))
                     
                     
                     myPred <- xts(select_(forecast_MA_df,
                                           quote(Mean),
                                           quote(Upper),
                                           quote(Lower)),
                                   order.by = ymd(forecast_MA_df$Date))
                     
                     xts_ma <- xts(ma_FC,
                                   order.by = as.Date(as.yearmon(time(ma_FC))))
                     
                     myDy <- cbind(myX, myPred, xts_ma)
                     
                     d <- dygraph(myDy[,1:5], main=paste0('MA FORECAST of: ', task_type, ' for ', forecast_n, ' periods' )) %>%
                       dyAxis("x", drawGrid = FALSE) %>%
                       dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                       dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                       dySeries('xts_ma', label = "past predictions") %>%
                       dySeries('series') %>%
                       dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                       dyLegend(width = 400) %>%
                       dyRangeSelector()
                     
                     print(d)
                   }
                 })
  })
  
  
  ###################################
  #### Simple Exponential Model #####
  ###################################
  
  
  output$p_SES <- renderDygraph({
    
    # Use existing reactive structures
    mySeries <- final_df()
    mySeries_SES <- mySeries_filtered()
    
    isolate({
      mySeries_SES <- mySeries_SES %>%
        filter(INCMONTH >= as.Date(input$date[1]) &
                 INCMONTH <= as.Date(input$date[2]))
    })
    
    if (nrow(mySeries_SES) == 0){
      stop(
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'))
      )
    }
    
    # Make inputs dependent on users hitting 'start forecasting' button
    isolate({
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-INCMONTH)))
        task_type = names(task_type[1])
      } else {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      recent_months <- input$i_recent_months
    })
    
    # Convert to TS object with monthly frequency
    myY <-  xts(select_(mySeries_SES, task_type),
                order.by=ymd(mySeries_SES$INCMONTH))
    
    # Set the start of the TS object until at the start of the first numeric observation
    for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
      if(is.na(myY[i])){
        next
      } else{
        myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
        break # Breaks the for loop once the first numeric observation is found
      }
    }
    

    
    withProgress(message = 'Generating Graph...  ',
                 detail = 'This may take a few seconds',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   if(nrow(myY) < 3){
                     return(NULL)
                   } else{
                     # Forecast n periods using model with selected confidence interval
                     isolate({
                       TS_mySeries_SES <- forecast::ses(myY,
                                                        h=forecast_n,
                                                        initial=c('optimal', 'simple'),
                                                        level = c(as.numeric(input$conf_int)))
                     })
                     
                     # Convert elements of time series FORECAST to dataframe for plotting
                     forecast_SES_df <- with(TS_mySeries_SES,
                                             data.frame(Mean=TS_mySeries_SES$mean,
                                                        Upper=TS_mySeries_SES$upper[,1],
                                                        Lower=TS_mySeries_SES$lower[,1]))
                     
                     # Add Date column to the forecasted values data.frame
                     forecast_SES_df$Date <- seq(as.Date(max(mySeries_SES$INCMONTH)) %m+% months(1),
                                                 by = "month",
                                                 length.out = forecast_n)
                     
                     forecast_SES_df <- forecast_SES_df %>%
                       select(Date, Mean, Upper, Lower) %>%
                       mutate(Mean = ifelse(as.integer(Mean) < 0,
                                            0,
                                            as.integer(round(Mean, 2))),
                              Upper = ifelse(as.integer(Upper) < 0,
                                             0,
                                             as.integer(round(Upper, 2))),
                              Lower = ifelse(as.integer(Lower) < 0,
                                             0,
                                             as.integer(round(Lower, 2))))
                     
                     y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                     m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                     y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                     m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                     tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                     
                     # Obtain the start and end date of the time series in the form of a ratio
                     timeProp <- tsp(tsmyY)[1]
                     timeProp2 <- tsp(tsmyY)[2]
                     # Create start and end point of holdout period
                     holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                     holdout_end <- nrow(myY)-forecast_n
                     prediction <- 0
                     j <- 1
                     
                     for (i in holdout_start:holdout_end){
                       
                       if(i < 2){
                         prediction[j] <- NA
                         j <- j+1
                       } else{
                         train <- window(tsmyY, end = timeProp + ((i-1)/12))
                         
                         if(nmonths(train) < 3){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           FC_ses <- forecast::ses(train,
                                                   h=forecast_n,
                                                   initial='optimal')
                           prediction[j] <- FC_ses[[2]][[forecast_n]]
                           j <- j+1
                         }
                       }
                     }
                     # Replace negative predictions with 0
                     prediction <- replace(prediction, prediction < 0, 0)
                     ses_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                     
                     ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                     myX <- xts(select_(mySeries_SES, series = task_type),
                                select_(mySeries_SES, quote(INCMONTH)),
                                order.by=ymd(mySeries_SES$INCMONTH))
                     
                     
                     myPred <- xts(select_(forecast_SES_df,
                                           quote(Mean),
                                           quote(Upper),
                                           quote(Lower)),
                                   order.by = ymd(forecast_SES_df$Date))
                     
                     xts_ses <- xts(ses_FC,
                                    order.by = as.Date(as.yearmon(time(ses_FC))))
                     
                     myDy <- cbind(myX, myPred, xts_ses)
                     
                     d <- dygraph(myDy[,1:5], main=paste0('SES FORECAST OF: ', task_type, ' for ', forecast_n, ' Periods' )) %>%
                       dyAxis("x", drawGrid = FALSE) %>%
                       dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                       dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                       dySeries('xts_ses', label = "past predictions") %>%
                       dySeries('series') %>%
                       dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                       dyLegend(width = 400) %>%
                       dyRangeSelector()
                     
                     print(d)
                   }
                 })
  })
  
  
  #############################################
  ############ Batch Forecasting ##############
  #############################################
  
  ###### ARIMA ######
  
  # Creates a datatable with arima forecasts when the batch forecast button is pressed
  arima <- eventReactive(input$batch_FC, {
    
    # Load the dataset created by the user 
    mySeries_arima <- final_df()
    
    # Filter the data by date as specified by the user
    mySeries_arima <- mySeries_arima %>% 
      filter(INCMONTH >= input$date[1] &
               INCMONTH <= input$date[2])
    
    # Create an xts object of the data chosen
    myY <- xts(mySeries_arima[,-1], 
               order.by = ymd(mySeries_arima$INCMONTH))
    
    # Allow the user to decide how many periods ahead to forecast
    isolate({
      forecast_n <- input$forecast_n
      recent_months <- input$i_recent_months2
      myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = 6), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
    })
    
    # Creates a loading bar as the forecast is created
    withProgress(message = 'Generating ARIMA Forecasts... ',
                 detail = 'This may take several minutes to hours depending on the selected data',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   if (input$ARIMAmodel){
                     Mean <- 0
                     Upper <- 0
                     Lower <- 0
                     for(i in 1:ncol(myY)){
                       
                       for(j in 1:nrow(myY)){
                         if(is.na(myY[j,i])){
                           next
                         } else{
                           myY2 <- myY[j:nrow(myY),i]
                           break
                         }
                       }
                       
                       arima_fcast <- forecast(auto.arima(myY2,
                                                          stepwise = FALSE,
                                                          approximation = TRUE),
                                               h = forecast_n,
                                               level = as.numeric(input$conf_int2))
                       Mean[i] <- arima_fcast$mean[forecast_n]
                       Upper[i] <- arima_fcast$upper[forecast_n]
                       Lower[i] <- arima_fcast$lower[forecast_n]
                     }
                     
                     
                     # rbind the point, upper, and lower forecast
                     arima_df <- as.data.frame(rbind(Mean, Upper, Lower))
                     # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                     arima_df <- as.data.frame(t(arima_df))
                     # Add the product name 
                     arima_df$Product <- colnames(myY)
                     # Add the date to the forecasts
                     arima_df$Date <- as.Date(max(mySeries_arima$INCMONTH)) %m+% months(forecast_n)
                     # Add the model name to the dataframe 
                     arima_df$Model <- "ARIMA"
                     
                     y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                     m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                     y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                     m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                     tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                     
                     # Create an empty matrix to store the recursive forecasts 
                     pred_ARIMA <- matrix(nrow = ncol(tsmyY), ncol = recent_months) 
                     # Create an empty matrix to store the month being forecasted
                     arima_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                     
                     # Recursive Forecast 
                     for (i in 1:ncol(tsmyY)){
                       k <- 1 # Set the index to 1 for each recursive forecast done on each column 
                       
                       # Subset each SKU to start at the first non-NA observation
                       for(z in 1:nrow(myY)){
                         if(is.na(myY[z,i])){
                           next
                         } else{
                           myY2 <- myY[z:nrow(tsmyY),i]
                           break
                         }
                       }
                       
                       # Convert new xts object to ts object
                       tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                       
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY2)-forecast_n
                       
                       timeProp <- tsp(tsmyY2)[1]
                       for (j in holdout_start:holdout_end){
                         
                         if(j < 2){
                           pred_ARIMA[i,k] <- NA
                           arima_months[i,k] <- NA
                           k <- k+1
                         } else{
                           train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                           
                           if(sum(!is.na(train)) < 3){
                             pred_ARIMA[i,k] <- NA
                             arima_months[i,k] <- NA
                             k <- k+1
                           } else{
                             FC_arima <- forecast(auto.arima(train, 
                                                             stepwise = FALSE, 
                                                             approximation = TRUE) , 
                                                  h = forecast_n) # Creates the model and forecasts
                             pred_ARIMA[i,k] <- FC_arima[[4]][[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                             arima_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                             k <- k+1 # Creates another index to store the next prediction
                           }
                         }
                       }
                     }
                     
                     # Convert the month matrix to a dataframe
                     arima_month_df <- arima_months %>%
                       as.data.frame()
                     
                     # Extract the first non-NA month from each SKU using forloop
                     Holdout_Start <- matrix(ncol = 1, nrow = nrow(arima_month_df))
                     for(i in 1:nrow(arima_month_df)){
                       for(j in 1:ncol(arima_month_df)){
                         if(all(is.na(arima_month_df[i,]))){
                           Holdout_Start[i,1] <- NA
                         } else{
                           if(is.na(arima_month_df[i,j])){
                             next
                           } else{
                             Holdout_Start[i,1] <- arima_month_df[i,j]
                             break
                           }
                         }
                       }
                     }
                     # Convert matrix to dataframe 
                     Holdout_Start <- as.data.frame(Holdout_Start)
                     # Rename column to Holdout_Start
                     colnames(Holdout_Start) <- "Holdout_Start"
                     
                     # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0 
                     pred_ARIMA_DF <- pred_ARIMA %>% 
                       t() %>%
                       as.data.frame() %>% 
                       apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                       apply(2, function(x) round(x, 2)) %>% 
                       apply(2, function(x) as.integer(x)) %>% 
                       as.data.frame()
                     # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                     timeProp2 <- tsp(tsmyY)[2] 
                     pred_ARIMA_TS <- pred_ARIMA_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                     
                     # Create an empty matrix to store accuracy measurement results 
                     my_arima_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function 
                     
                     # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements 
                     for (i in 1:ncol(tsmyY)){
                       my_arima_accuracy[i,] <- pred_ARIMA_TS[, i] %>% accuracy(tsmyY[,i])
                     }
                     
                     
                      my_arima_accuracy_DF <- as.data.frame(my_arima_accuracy[, c(2,3,5)])
                      colnames(my_arima_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                     
                     
                     # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                     arima_df_final <- cbind(arima_df, my_arima_accuracy_DF, Holdout_Start)
                     # Convert holdout_start to yearmon format
                     arima_df_final$Holdout_Start <- as.yearmon(arima_df_final$Holdout_Start)
                     # Remove the phrase _actuals.mean from the Product name 
                     arima_df_final$Product <- gsub("_Billable.Hours", " ", arima_df_final$Product)
                     # Remove the date that is displayed in column name in certain ARIMA forecasts
                     arima_df_final$Product <- gsub("\\..*", "", arima_df_final$Product)
                     # Convert to Mean, Upper and Lower to integers
                     arima_df_final$Upper <- as.integer(arima_df_final$Upper)
                     arima_df_final$Mean <- as.integer(arima_df_final$Mean)
                     arima_df_final$Lower <- as.integer(arima_df_final$Lower)
                     
                     arima_df_final <- arima_df_final %>%
                       select(Product, Model, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                       mutate(Mean = ifelse(Mean < 0,
                                            0,
                                            round(Mean, 2)),
                              Upper = ifelse(Upper < 0,
                                             0,
                                             round(Upper, 2)),
                              Lower = ifelse(Lower < 0,
                                             0,
                                             round(Lower, 2)),
                              RMSE = round(RMSE, 2),
                              MAE = round(MAE, 2),
                              MAPE = round(MAPE, 2),
                              Holdout_Start = as.character(Holdout_Start))
                     
                     return(arima_df_final)
                   } else {
                     return(NULL)
                   }
                 })
  })
  
  ####################################################################################################################################
  
  ###### ERROR TREND SEASONAL MODEL ######
  
  ets_df <- eventReactive(input$batch_FC, {
    mySeries_ets <- final_df()
    
    mySeries_ets <- mySeries_ets %>%
      filter(INCMONTH >= input$date[1] &
               INCMONTH <= input$date[2])
    
    myY <- xts(mySeries_ets[,-1], 
               order.by = ymd(mySeries_ets$INCMONTH))
    
    isolate({
      forecast_n <- input$forecast_n
      recent_months <- input$i_recent_months2
      myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = 6), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
    })
    
    withProgress(message = 'Generating ETS Forecasts... ',
                 detail = 'This may take several minutes to hours depending on the selected data',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   if (input$ETSmodel){
                     Mean <- 0
                     Upper <- 0
                     Lower <- 0
                     Parameters <- matrix(nrow=ncol(myY), ncol = 4)
                     for(i in 1:ncol(myY)){
                       
                       # Set the start of the xts object to the first non-NA observation
                       for(j in 1:nrow(myY)){
                         if(is.na(myY[j,i])){
                           next
                         } else{
                           myY2 <- myY[j:nrow(myY),i]
                           break
                         }
                       }
                       
                       if(sum(!is.na(myY2)) < 3){
                         Mean[i] <- NA
                         Upper[i] <- NA
                         Lower[i] <- NA
                         Parameters[i,] <- NA
                       } else{
                         ets_fcast <- forecast(ets(myY2,
                                                   allow.multiplicative.trend = TRUE,
                                                   opt.crit = c("lik", "amse", "mse", "sigma", "mae")),
                                               h = forecast_n,
                                               level = as.numeric(input$conf_int2))
                         Mean[i] <- ets_fcast$mean[forecast_n]
                         Upper[i] <- ets_fcast$upper[forecast_n]
                         Lower[i] <- ets_fcast$lower[forecast_n]
                         Parameters[i,] <- ets_fcast$model$components
                       }
                     }
                     
                    
                     # rbind the point, upper, and lower forecast
                     ets_df <- as.data.frame(rbind(Mean, Upper, Lower))
                     # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                     ets_df <- as.data.frame(t(ets_df))
                     # Add the product names 
                     ets_df$Product <- colnames(myY)
                     # Add the date to the forecasts
                     ets_df$Date <- as.Date(max(mySeries_ets$INCMONTH)) %m+% months(forecast_n)
                     # Add the model name to the dataframe
                     ets_df$Model <- "ETS" 
                     
                     y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                     m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                     y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                     m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                     tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                     
                     # Create an empty matrix to store the recursive forecasts
                     pred_ets <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                     # Create an empty matrix to store the month being forecasted
                     ets_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                     
                     # Recursive Forecast 
                     for (i in 1:ncol(tsmyY)){
                       k <- 1 # Set the index to 1 for each recursive forecast done on each column 
                       
                       # Subset each SKU to start at the first non-NA observation
                       for(z in 1:nrow(myY)){
                         if(is.na(myY[z,i])){
                           next
                         } else{
                           myY2 <- myY[z:nrow(myY),i]
                           break
                         }
                       }
                       
                       # Convert xts object to ts object
                       tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                       
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY2)-forecast_n
                       
                       timeProp <- tsp(tsmyY2)[1]
                       for (j in holdout_start:holdout_end){
                         
                         if(j < 2){
                           pred_ets[i,k] <- NA
                           ets_months[i,k] <- NA
                           k <- k+1
                         } else{
                           train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                           
                           if(sum(!is.na(train)) < 3){
                             pred_ets[i,k] <- NA
                             ets_months[i,k] <- NA
                             k <- k+1
                           } else{
                             FC_ets <- forecast(ets(train,
                                                    allow.multiplicative.trend = TRUE,
                                                    opt.crit = c("lik", "amse", "mse", "sigma", "mae")),
                                                h = forecast_n)
                             pred_ets[i,k] <- FC_ets[[2]][[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                             ets_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                             k <- k+1 # Creates another index to store the next prediction
                           }
                         }
                       }
                     }
                     
                     # Convert the month matrix to a dataframe
                     ets_month_df <- ets_months %>%
                       as.data.frame()
                     # Extract the first non-NA month from each SKU using forloop
                     Holdout_Start <- matrix(ncol = 1, nrow = nrow(ets_month_df))
                     for(i in 1:nrow(ets_month_df)){
                       for(j in 1:ncol(ets_month_df)){
                         if(all(is.na(ets_month_df[i,]))){
                           Holdout_Start[i,1] <- NA
                         } else{
                           if(is.na(ets_month_df[i,j])){
                             next
                           } else{
                             Holdout_Start[i,1] <- ets_month_df[i,j]
                             break
                           }
                         }
                       }
                     }
                     # Convert matrix to dataframe 
                     Holdout_Start <- as.data.frame(Holdout_Start)
                     # Rename column to Holdout_Start
                     colnames(Holdout_Start) <- "Holdout_Start"
                     
                     # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0
                     pred_ets_DF <- pred_ets %>% 
                       t() %>%
                       as.data.frame() %>% 
                       apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                       apply(2, function(x) round(x, 2)) %>% 
                       apply(2, function(x) as.integer(x)) %>% 
                       as.data.frame()
                     # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                     timeProp2 <- tsp(tsmyY)[2] 
                     pred_ets_TS <- pred_ets_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                     
                     # Create an empty matrix to store accuracy measurement results
                     my_ets_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                     
                     # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                     for (i in 1:ncol(tsmyY)){
                       my_ets_accuracy[i,] <- pred_ets_TS[, i] %>% accuracy(tsmyY[,i])
                     }
                     
                       
                     
                      my_ets_accuracy_DF <- as.data.frame(my_ets_accuracy[, c(2,3,5)])
                      colnames(my_ets_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                     
                     
                     # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                     ets_df_final <- cbind(ets_df, my_ets_accuracy_DF, Holdout_Start)
                     # Convert holdout_start to yearmon format
                     ets_df_final$Holdout_Start <- as.yearmon(ets_df_final$Holdout_Start)
                     # Remove the phrase _actuals.mean from the Product name 
                     ets_df_final$Product <- gsub("_Billable.Hours", " ", ets_df_final$Product)
                     # Convert to Mean, Upper and Lower to integers
                     ets_df_final$Upper <- as.integer(ets_df_final$Upper)
                     ets_df_final$Mean <- as.integer(ets_df_final$Mean)
                     ets_df_final$Lower <- as.integer(ets_df_final$Lower)
                     
                     ets_df_final <- ets_df_final %>%
                       select(Product, Model,Date, Mean, Upper,Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                       mutate(Mean = ifelse(Mean < 0,
                                            0,
                                            round(Mean, 2)),
                              Upper = ifelse(Upper < 0,
                                             0,
                                             round(Upper, 2)),
                              Lower = ifelse(Lower < 0,
                                             0,
                                             round(Lower, 2)),
                              RMSE = round(RMSE, 2),
                              MAE = round(MAE, 2),
                              MAPE = round(MAPE, 2),
                              Holdout_Start = as.character(Holdout_Start))
                     return(ets_df_final)
                   } else {
                     return(NULL)
                   }
                 })
  })

  
  ####################################################################################################################################
  
  ###### MOVING AVERAGE ######
  
  ma_df <- eventReactive(input$batch_FC, {
    
    mySeries_ma <- final_df()
    
    mySeries_ma <- mySeries_ma %>%
      filter(INCMONTH >= input$date[1] &
               INCMONTH <= input$date[2])
    
    myY <- xts(mySeries_ma[,-1], 
               order.by = ymd(mySeries_ma$INCMONTH))
    
    isolate({
      forecast_n <- input$forecast_n
      recent_months <- input$i_recent_months2
      myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = 6), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      
    })
    
    withProgress(message = 'Generating Moving Average Forecasts... ',
                 detail = 'This may take several minutes to hours depending on the selected data',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   if (input$MAmodel){
                     Mean <- 0
                     Upper <- 0
                     Lower <- 0
                     Parameters <- 0
                     for(i in 1:ncol(myY)){
                       
                       # Set the start of the xts object to the first non-NA observation
                       for(j in 1:nrow(myY)){
                         if(is.na(myY[j,i])){
                           next
                         } else{
                           myY2 <- myY[j:nrow(myY),i]
                           break
                         }
                       }
                       
                       if(nrow(myY2) < 3){
                         Mean[i] <- NA
                         Upper[i] <- NA
                         Lower[i] <- NA
                         Parameters[i] <- NA
                       } else{
                         ma_fcast <- forecast(auto.arima(myY2,
                                                         max.p = 0,
                                                         stationary = TRUE,
                                                         seasonal = FALSE),
                                              h = forecast_n,
                                              level = as.numeric(input$conf_int2))
                         Mean[i] <- ma_fcast$mean[forecast_n]
                         Upper[i] <- ma_fcast$upper[forecast_n]
                         Lower[i] <- ma_fcast$lower[forecast_n]
                         Parameters[i] <- ma_fcast$method
                       }
                     }
                     
                     # rbind the point, upper, and lower forecast
                     ma_df <- as.data.frame(rbind(Mean, Upper, Lower))
                     # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                     ma_df <- as.data.frame(t(ma_df))
                     # Add the product name 
                     ma_df$Product <- colnames(myY)
                     # Add the date to the forecasts
                     ma_df$Date <- as.Date(max(mySeries_ma$INCMONTH)) %m+% months(forecast_n)
                     # Add the model name to the dataframe
                     ma_df$Model <- "MA"
                     
                     y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                     m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                     y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                     m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                     tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                     
                     # Create an empty matrix to store the recursive forecasts
                     pred_ma <- matrix(nrow = ncol(tsmyY), ncol = recent_months) 
                     # Create an empty matrix to store the month being forecasted
                     ma_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                     
                     for (i in 1:ncol(tsmyY)){
                       k <- 1
                       
                       # Subset each SKU to start at the first non-NA observation
                       for(z in 1:nrow(myY)){
                         if(is.na(myY[z,i])){
                           next
                         } else{
                           myY2 <- myY[z:nrow(tsmyY),i]
                           break
                         }
                       }
                       
                       # Convert new xts object to ts object
                       tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                       
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY2)-forecast_n
                       
                       timeProp <- tsp(tsmyY2)[1]
                       for (j in holdout_start:holdout_end){
                         
                         if(j < 2){
                           pred_ma[i,k] <- NA
                           ma_months[i,k] <- NA
                           k <- k+1
                         } else{
                           train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                           
                           if(sum(!is.na(train)) < 3) {
                             pred_ma[i,k] <- NA
                             ma_months[i,k] <- NA
                             k <- k+1
                           } else{
                             FC_ma <- forecast(auto.arima(train, 
                                                          max.p = 0, 
                                                          stationary = TRUE, 
                                                          seasonal = FALSE) , 
                                               h = forecast_n) # Creates the model and forecasts
                             pred_ma[i,k] <- FC_ma[[4]][[forecast_n]] #Store the predicted forecast in the vector prediction with index j
                             ma_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                             k <- k+1 # Creates another index to store the next prediction
                           }
                         }
                       }
                     }
                     
                     # Convert the month matrix to a dataframe
                     ma_month_df <- ma_months %>%
                       as.data.frame()
                     # Extract the first non-NA month from each SKU using forloop
                     Holdout_Start <- matrix(ncol = 1, nrow = nrow(ma_month_df))
                     for(i in 1:nrow(ma_month_df)){
                       for(j in 1:ncol(ma_month_df)){
                         if(all(is.na(ma_month_df[i,]))){
                           Holdout_Start[i,1] <- NA
                         } else{
                           if(is.na(ma_month_df[i,j])){
                             next
                           } else{
                             Holdout_Start[i,1] <- ma_month_df[i,j]
                             break
                           }
                         }
                       }
                     }
                     # Convert matrix to dataframe 
                     Holdout_Start <- as.data.frame(Holdout_Start)
                     # Rename column to Holdout_Start
                     colnames(Holdout_Start) <- "Holdout_Start"
                     
                     # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0 
                     pred_ma_DF <- pred_ma %>% 
                       t() %>%
                       as.data.frame() %>% 
                       apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                       apply(2, function(x) round(x, 2)) %>% 
                       apply(2, function(x) as.integer(x)) %>% 
                       as.data.frame()
                     # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                     timeProp2 <- tsp(tsmyY)[2] 
                     pred_ma_TS <- pred_ma_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                     
                     # Create an empty matrix to store accuracy measurement results
                     my_ma_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                     
                     # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                     for (i in 1:ncol(tsmyY)){
                       my_ma_accuracy[i,] <- pred_ma_TS[, i] %>% accuracy(tsmyY[,i])
                     }
                     
                  
                    
                      my_ma_accuracy_DF <- as.data.frame(my_ma_accuracy[, c(2,3,5)])
                      colnames(my_ma_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                     
                     
                     # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                     ma_df_final <- cbind(ma_df, my_ma_accuracy_DF, Holdout_Start)
                     # Convert holdout_start to yearmon format
                     ma_df_final$Holdout_Start <- as.yearmon(ma_df_final$Holdout_Start)
                     # Remove the phrase _actuals.mean from the Product name 
                     ma_df_final$Product <- gsub("_Billable.Hours", " ", ma_df_final$Product)
                     # Remove the date that is displayed in column name in certain ARIMA forecasts
                     ma_df_final$Product <- gsub("\\..*", "", ma_df_final$Product)
                     # Convert to Mean, Upper and Lower to integers
                     ma_df_final$Upper <- as.integer(ma_df_final$Upper)
                     ma_df_final$Mean <- as.integer(ma_df_final$Mean)
                     ma_df_final$Lower <- as.integer(ma_df_final$Lower)
                     
                     ma_df_final <- ma_df_final %>%
                       select(Product, Model,Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                       mutate(Mean = ifelse(Mean < 0,
                                            0,
                                            round(Mean, 2)),
                              Upper = ifelse(Upper < 0,
                                             0,
                                             round(Upper, 2)),
                              Lower = ifelse(Lower < 0,
                                             0,
                                             round(Lower, 2)),
                              RMSE = round(RMSE, 2),
                              MAE = round(MAE, 2),
                              MAPE = round(MAPE, 2),
                              Holdout_Start = as.character(Holdout_Start))
                     return(ma_df_final)
                   } else {
                     return(NULL)
                   }
                 })
  })
  
  ####################################################################################################################################
  
  ###### SIMPLE EXPONENTIAL SMOOTHING ######
  
  ses <- eventReactive(input$batch_FC, {
    
    mySeries_ses <- final_df()
    
    mySeries_ses <- mySeries_ses %>%
      filter(INCMONTH >= input$date[1] &
               INCMONTH <= input$date[2])
    
    myY <- xts(mySeries_ses[,-1], 
               order.by = ymd(mySeries_ses$INCMONTH))
    
    isolate({
      forecast_n <- input$forecast_n
      recent_months <- input$i_recent_months2
      myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = 6), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      
    })
    
    withProgress(message = 'Generating SES Forecasts... ',
                 detail = 'This may take several minutes to hours depending on the selected data',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   if (input$SESmodel){
                     Mean <- 0
                     Upper <- 0
                     Lower <- 0
                     Parameters <- matrix(nrow=ncol(myY), ncol = 4)
                     for(i in 1:ncol(myY)){
                       
                       # Set the start of the xts object to the first non-NA observation
                       for(j in 1:nrow(myY)){
                         if(is.na(myY[j,i])){
                           next
                         } else{
                           myY2 <- myY[j:nrow(myY),i]
                           break
                         }
                       }
                       
                       if(sum(!is.na(myY2)) < 3){
                         Mean[i] <- NA
                         Upper[i] <- NA
                         Lower[i] <- NA
                         Parameters[i,] <- NA
                       } else{
                         ses_fcast <- forecast::ses(myY2,
                                                    initial = "optimal",
                                                    h = forecast_n,
                                                    level = as.numeric(input$conf_int2))
                         Mean[i] <- ses_fcast$mean[forecast_n]
                         Upper[i] <- ses_fcast$upper[forecast_n]
                         Lower[i] <- ses_fcast$lower[forecast_n]
                         Parameters[i,] <- ses_fcast$model$components
                       }
                     }
                     
                     # rbind the point, upper, and lower forecast
                     ses_df <- as.data.frame(rbind(Mean, Upper, Lower))
                     # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                     ses_df <- as.data.frame(t(ses_df))
                     # Add the product names 
                     ses_df$Product <- colnames(myY)
                     # Add the date to the forecasts
                     ses_df$Date <- as.Date(max(mySeries_ses$INCMONTH)) %m+% months(forecast_n)
                     # Add the model name to the dataframe
                     ses_df$Model <- "SES"
                     
                     y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                     m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                     y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                     m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                     tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                     
                     # Create an empty matrix to store the recursive forecasts
                     pred_ses <- matrix(nrow = ncol(tsmyY), ncol = recent_months) 
                     # Create an empty matrix to store the month being forecasted
                     ses_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                     
                     # Recursive Forecast
                     for (i in 1:ncol(tsmyY)){
                       k <- 1
                       
                       # Subset each SKU to start at the first non-NA observation
                       for(z in 1:nrow(myY)){
                         if(is.na(myY[z,i])){
                           next
                         } else{
                           myY2 <- myY[z:nrow(myY),i]
                           break
                         }
                       }
                       
                       # Convert xts object to ts object
                       tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                       
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY2)-forecast_n
                       
                       timeProp <- tsp(tsmyY2)[1]
                       for (j in holdout_start:holdout_end){
                         
                         if(j < 2){
                           pred_ses[i,k] <- NA
                           ses_months[i,k] <- NA
                           k <- k+1
                         } else{
                           train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                           
                           if(sum(!is.na(train)) < 3){
                             pred_ses[i,k] <- NA
                             ses_months[i,k] <- NA
                             k <- k+1
                           } else{
                             FC_ses <- forecast::ses(train,
                                                     h=forecast_n,
                                                     initial=c('optimal', 'simple'))
                             pred_ses[i,k] <- FC_ses[[2]][[forecast_n]] #Store the predicted forecast in the vector prediction with index j
                             ses_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                             k <- k+1 # Creates another index to store the next prediction
                           }
                         }
                       }
                     }
                     
                     # Convert the month matrix to a dataframe
                     ses_month_df <- ses_months %>%
                       as.data.frame()
                     # Extract the first non-NA month from each SKU using forloop
                     Holdout_Start <- matrix(ncol = 1, nrow = nrow(ses_month_df))
                     for(i in 1:nrow(ses_month_df)){
                       for(j in 1:ncol(ses_month_df)){
                         if(all(is.na(ses_month_df[i,]))){
                           Holdout_Start[i,1] <- NA
                         } else{
                           if(is.na(ses_month_df[i,j])){
                             next
                           } else{
                             Holdout_Start[i,1] <- ses_month_df[i,j]
                             break
                           }
                         }
                       }
                     }
                     # Convert matrix to dataframe 
                     Holdout_Start <- as.data.frame(Holdout_Start)
                     # Rename column to Holdout_Start
                     colnames(Holdout_Start) <- "Holdout_Start"
                     
                     
                     # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0 
                     pred_ses_DF <- pred_ses %>% 
                       t() %>%
                       as.data.frame() %>% 
                       apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                       apply(2, function(x) round(x, 2)) %>% 
                       apply(2, function(x) as.integer(x)) %>% 
                       as.data.frame()
                     # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                     timeProp2 <- tsp(tsmyY)[2] 
                     pred_ses_TS <- pred_ses_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                     
                     # Create an empty matrix to store accuracy measurement results
                     my_ses_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                     
                     # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                     for (i in 1:ncol(tsmyY)){
                       my_ses_accuracy[i,] <- pred_ses_TS[, i] %>% accuracy(tsmyY[,i])
                     }
                     
                     
                    
                    my_ses_accuracy_DF <- as.data.frame(my_ses_accuracy[, c(2,3,5)])
                    colnames(my_ses_accuracy_DF) <- c("RMSE", "MAE", "MAPE")

                     
                     # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                     ses_df_final <- cbind(ses_df, my_ses_accuracy_DF, Holdout_Start)
                     # Convert holdout_start to yearmon format
                     ses_df_final$Holdout_Start <- as.yearmon(ses_df_final$Holdout_Start)
                     # Remove the phrase _actuals.mean from the Product name 
                     ses_df_final$Product <- gsub("_Billable.Hours", " ", ses_df_final$Product)
                     # Convert to Mean, Upper and Lower to integers
                     ses_df_final$Upper <- as.integer(ses_df_final$Upper)
                     ses_df_final$Mean <- as.integer(ses_df_final$Mean)
                     ses_df_final$Lower <- as.integer(ses_df_final$Lower)
                     
                     ses_df_final <- ses_df_final %>%
                       select(Product, Model, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                       mutate(Mean = ifelse(Mean < 0,
                                            0,
                                            round(Mean, 2)),
                              Upper = ifelse(Upper < 0,
                                             0,
                                             round(Upper, 2)),
                              Lower = ifelse(Lower < 0,
                                             0,
                                             round(Lower, 2)),
                              RMSE = round(RMSE, 2),
                              MAE = round(MAE, 2),
                              MAPE = round(MAPE, 2),
                              Holdout_Start = as.character(Holdout_Start))
                     return(ses_df_final)
                   } else {
                     return(NULL)
                   }
                 })
  })
  
  ###############################################################################################################################################################################
  
  output$final_batchdf <- DT::renderDataTable(extensions = "Scroller", 
                                              options = list(deferRender = TRUE,
                                                             scrollX = TRUE,
                                                             scrollY = 700,
                                                             scroller = TRUE),{
                                                               
                                                               arima <- arima()
                                                               ets <- ets_df()
                                                               ma <- ma_df()
                                                               ses <- ses()
                                                               
                                                               
                                                               final_batchdf <- as.data.frame(rbind(arima, ets, ma, ses)) %>% 
                                                                 arrange(Product, Model)
                                                               
                                                               final_batchdf <- datatable(final_batchdf) 
                                                               
                                                               return(final_batchdf)
                                                             })
  
  model_recommend <- reactive({
    arima <- arima()
    ets <- ets_df()
    ma <- ma_df()
    ses <- ses()

    recommend_df <- as.data.frame(rbind(arima, ets,  ma, ses)) %>%
      arrange(Product, Model)
    
    if(input$error_measurement == "RMSE"){
      recommend_df <- recommend_df %>%
        group_by(Product) %>%
        filter(is.finite(RMSE) == TRUE,
               RMSE == min(RMSE, na.rm = TRUE))
    } else if(input$error_measurement == "MAE"){
      recommend_df <- recommend_df %>%
        group_by(Product) %>%
        filter(is.finite(MAE) == TRUE,
          MAE == min(MAE, na.rm = TRUE))
    } else {
      recommend_df <- recommend_df %>%
        group_by(Product) %>%
        filter(is.finite(MAPE) == TRUE,
               MAPE == min(MAPE, na.rm = TRUE))
    }
    return(datatable(recommend_df))
  })
  
  output$model_recommend_df <- DT::renderDataTable(extensions = "Scroller",
                                                   options = list(deferRender = TRUE,
                                                                  scrollY = 700,
                                                                  scroller = TRUE),{
                                                                    model_recommend()
                                                                  })
  
  ###############################################################################################################################################################################
  output$model_performance <- DT::renderDataTable(extensions = "Scroller",
                                                  options = list(deferRender = TRUE,
                                                                 scrollY = 700,
                                                                 scroller = TRUE),{
                                                                   
                                                                   # Use existing reactive structures
                                                                   mySeries <- final_df()
                                                                   mySeries_filtered <- mySeries_filtered()
                                                                   
                                                                   isolate({
                                                                     mySeries_filtered <- mySeries_filtered %>%
                                                                       filter(INCMONTH >= input$date[1] &
                                                                                INCMONTH <= input$date[2])
                                                                   })
                                                                   
                                                                   ################################################################################################# 
                                                                   
                                                                   if (nrow(mySeries_filtered) == 0){
                                                                     stop(
                                                                       showModal(modalDialog(
                                                                         title = "Important message",
                                                                         'Please hit "start forecasting"!',
                                                                         easyClose = TRUE,
                                                                         size = 's'))
                                                                     )
                                                                   }
                                                                   
                                                                   ################################################################################################# 
                                                                   
                                                                   # Make inputs dependent on users hitting 'start forecasting' button
                                                                   isolate({
                                                                     if(input$i_task_select ==""){
                                                                       task_type = select_(mySeries,
                                                                                           .dots = list(quote(-INCMONTH)))
                                                                       task_type = names(task_type[1])
                                                                     } else {
                                                                       task_type = input$i_task_select
                                                                     }
                                                                     forecast_n <- input$i_forecast_n
                                                                     recent_months <- input$i_recent_months
                                                                   })
                                                                   
                                                                   #################################################################################################
                                                                   
                                                                   # Convert to TS object with monthly frequency
                                                                   myY <-  xts(select_(mySeries_filtered,
                                                                                       task_type),
                                                                               order.by=ymd(mySeries_filtered$INCMONTH))
                                                                   
                                                                   # Set the start of the TS object until at the start of the first numeric observation
                                                                   for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
                                                                     if(is.na(myY[i])){
                                                                       next
                                                                     } else{
                                                                       myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
                                                                       break # Breaks the for loop once the first numeric observation is found
                                                                     }
                                                                   }
                                                                   
                                                                  
                                                                   
                                                                   #################################################################################################
                                                                   
                                                                   withProgress(message = 'Generating Forecasts...  ',
                                                                                detail = 'this may take a few seconds',
                                                                                value = 0.1,
                                                                                min = 0,
                                                                                max = 1, {
                                                                                  
                                                                                  # Convert xts object to ts object
                                                                                  y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                                                                                  m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                                                                                  y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                                                                                  m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                                                                                  
                                                                                  #################################################################################################
                                                                                  
                                                                                  tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                                                                                  #tsmyY2 <- replace(tsmyY, tsmyY == 0, 1) # Replaces 0s in the ts with .01 to calculate MAPE later on
                                                                                  
                                                                                  # Create start and end point of holdout period
                                                                                  holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                                                                                  holdout_end <- nrow(myY)-forecast_n
                                                                                  
                                                                                  timeProp <- tsp(tsmyY)[1] # Takes the first observation and creates a numerical representation of the date
                                                                                  timeProp2 <- tsp(tsmyY)[2] # Takes the last observation and creates a numerical representation of the date
                                                                                  
                                                                                  #################################################################################################
                                                                                  
                                                                                  prediction_arima <- 0 # Set the vector 0 to store predicted values
                                                                                  prediction_ets <- 0 # Set the vector 0 to store predicted values
                                                                                  
                                                                                  prediction_ma <- 0 # Set the vector 0 to store predicted values
                                                                                  prediction_ses <- 0 # Set the vector 0 to store predicted values
                                                                                  
                                                                                  arima_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                                                                                  ets_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                                                                                
                                                                                  ma_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                                                                                  ses_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                                                                                  
                                                                                  j <- 1 # Sets the first index to 1 to store predicted values
                                                                                  m <- 1 # Sets the first index to 1 to store predicted values
                                                                                  
                                                                                  s <- 1 # Sets the first index to 1 to store predicted values
                                                                                  t <- 1 # Sets the first index to 1 to store predicted values
                                                                                  
                                                                                  #################################################################################################
                                                                                  
                                                                                  ### ARIMA MODEL ###
                                                                                  if(nrow(myY) < 3){
                                                                                    isolate({
                                                                                      forecast_Arima_df <- NULL
                                                                                      my_arima_Accuracy <- NULL
                                                                                    })
                                                                                  } else{
                                                                                    isolate({
                                                                                      # Forecast n periods using model with selected confidence intervals
                                                                                      TS_mySeries_ARIMA <- forecast(auto.arima(myY,
                                                                                                                               stepwise = FALSE,
                                                                                                                               approximation = TRUE),
                                                                                                                    h = forecast_n,
                                                                                                                    level = as.numeric(input$conf_int)
                                                                                      )
                                                                                    })
                                                                                    # Convert elements of time series FORECAST to dataframe
                                                                                    forecast_ARIMA_df <- with(TS_mySeries_ARIMA,
                                                                                                              data.frame(Mean=TS_mySeries_ARIMA$mean[forecast_n],
                                                                                                                         Upper=TS_mySeries_ARIMA$upper[forecast_n, 1],
                                                                                                                         Lower=TS_mySeries_ARIMA$lower[forecast_n, 1]
                                                                                                              ))
                                                                                    
                                                                                    # Add Date column to the forecasted values data.frame
                                                                                    forecast_ARIMA_df$Date <- as.Date(max(mySeries_filtered$INCMONTH)) %m+% months(forecast_n)
                                                                                    
                                                                                    # Add column for model name
                                                                                    forecast_ARIMA_df$Model <- "ARIMA"
                                                                                    
                                                                                    # Adjust dataframe to replace negative forecasted values with 0
                                                                                    forecast_ARIMA_df <- forecast_ARIMA_df %>%
                                                                                      select(Model,Date, Mean, Upper, Lower) %>%
                                                                                      mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                                                                           0,
                                                                                                           as.integer(round(Mean, 2))),
                                                                                             Upper = ifelse(as.integer(Upper) < 0,
                                                                                                            0,
                                                                                                            as.integer(round(Upper, 2))),
                                                                                             Lower = ifelse(as.integer(Lower) < 0,
                                                                                                            0,
                                                                                                            as.integer(round(Lower, 2))))
                                                                                    
                                                                                    # Recursive forecast for ARIMA model
                                                                                    for (i in holdout_start:holdout_end){
                                                                                      
                                                                                      if(i < 2){
                                                                                        prediction_arima[j] <- NA
                                                                                        arima_months[j] <- NA
                                                                                        j <- j+1
                                                                                      } else{
                                                                                        train <- window(tsmyY, end = timeProp + ((i-1)/12)) # Creates the first training dataset to use for forecasting
                                                                                        
                                                                                        if(nmonths(train) < 3){
                                                                                          prediction_arima[j] <- NA
                                                                                          arima_months[j] <- NA
                                                                                          j <- j+1
                                                                                        } else{
                                                                                          FC_arima <- forecast(auto.arima(train, 
                                                                                                                          stepwise = FALSE, 
                                                                                                                          approximation = TRUE) , 
                                                                                                               h = forecast_n) # Creates the model and forecasts
                                                                                          prediction_arima[j] <- FC_arima[[4]][[forecast_n]] #Store the predicted forecast in the vector prediction with index j
                                                                                          arima_months[j] <- tsp(train)[2] + ((forecast_n)/12)
                                                                                          j <- j+1 # Creates another index to store the next prediction
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    
                                                                                    # Convert the month matrix to a dataframe
                                                                                    arima_month_df <- arima_months %>%
                                                                                      as.data.frame()
                                                                                    # Extract the first non-NA month from each SKU using forloop
                                                                                    arima_start <- 0
                                                                                    for(i in 1:nrow(arima_month_df)){
                                                                                      if(all(is.na(arima_month_df))){
                                                                                        arima_start <- NA 
                                                                                      } else{
                                                                                        if(is.na(arima_month_df[i,])){
                                                                                          next
                                                                                        } else{
                                                                                          arima_start <- arima_month_df[i,]
                                                                                          break
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    # Convert to yearmon format
                                                                                    arima_start <- as.yearmon(arima_start)
                                                                                    
                                                                                    # Replace negative predictions with 0
                                                                                    prediction_arima <- replace(prediction_arima, prediction_arima < 0, 0) %>% 
                                                                                      round(2) %>%
                                                                                      as.integer()
                                                                                    
                                                                                    # Converts the predictions into a TS object for plotting and measuring accuracy
                                                                                    arima_FC <- prediction_arima %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                                                                                    
                                                                                    # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                                                                                    my_arima_Accuracy <- arima_FC %>%
                                                                                      accuracy(tsmyY) 
                                                                                    
                                                                                    my_arima_Accuracy <- my_arima_Accuracy[,c(2,3,5)] %>% # Subsets RMSE, MAE, and MAPE accuracy measurements
                                                                                      as.data.frame() %>% # Converts accuracy measurements to a dataframe
                                                                                      t() # Transposes the dataset to have RMSE, MAE, and MAPE as the column names
                                                                                    rownames(my_arima_Accuracy) <- NULL # Removes the rownames
                                                                                    
                                                                                    # Add Holdout_Start to the df
                                                                                    my_arima_Accuracy$Holdout_Start <- arima_start # Coercess to list instead of dataframe
                                                                                    
                                                                                    # Convert back to dataframe
                                                                                    my_arima_Accuracy <- as.data.frame(my_arima_Accuracy)
                                                                                    
                                                                                    # Rename columns
                                                                                    colnames(my_arima_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                                                                                  }
                                                                                  
                                                                                  
                                                                                  
                                                                                  ### ETS Model ### 
                                                                                  if(nrow(myY) < 3){
                                                                                    forecast_ets_df <- NULL
                                                                                    my_ets_Accuracy <- NULL
                                                                                  } else{
                                                                                    isolate({
                                                                                      TS_mySeries_ets <- forecast(ets(myY,
                                                                                                                      allow.multiplicative.trend = TRUE,
                                                                                                                      opt.crit = c("lik", "amse", "mse", "sigma", "mae")),
                                                                                                                  h = forecast_n,
                                                                                                                  level = as.numeric(input$conf_int)
                                                                                      )
                                                                                    })
                                                                                    
                                                                                    # Convert elements of time series FORECAST to dataframe 
                                                                                    forecast_ets_df <- with(TS_mySeries_ets,
                                                                                                            data.frame(Mean=TS_mySeries_ets$mean[forecast_n],
                                                                                                                       Upper=TS_mySeries_ets$upper[forecast_n, 1],
                                                                                                                       Lower=TS_mySeries_ets$lower[forecast_n, 1]))
                                                                                    
                                                                                    forecast_ets_df$Date <- as.Date(max(mySeries_filtered$INCMONTH)) %m+% months(forecast_n)
                                                                                    
                                                                                    # Add column for model name
                                                                                    forecast_ets_df$Model <- "ETS"
                                                                                    
                                                                                    # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                                                                                    forecast_ets_df <- forecast_ets_df %>%
                                                                                      select(Model, Date, Mean, Upper, Lower) %>%
                                                                                      mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                                                                           0,
                                                                                                           as.integer(round(Mean, 2))),
                                                                                             Upper = ifelse(as.integer(Upper) < 0,
                                                                                                            0,
                                                                                                            as.integer(round(Upper, 2))),
                                                                                             Lower = ifelse(as.integer(Lower) < 0,
                                                                                                            0,
                                                                                                            as.integer(round(Lower, 2)))
                                                                                      )
                                                                                    
                                                                                    # Recursive forecast for ETS model
                                                                                    for (i in holdout_start:holdout_end){
                                                                                      
                                                                                      if(i < 2){
                                                                                        prediction_ets[m] <- NA
                                                                                        ets_months[m] <- NA
                                                                                        m <- m+1
                                                                                      } else{
                                                                                        train <- window(tsmyY, end = timeProp + ((i-1)/12))
                                                                                        
                                                                                        if(nmonths(train) < 3){
                                                                                          prediction_ets[m] <- NA
                                                                                          ets_months[m] <- NA
                                                                                          m <- m+1
                                                                                        } else{
                                                                                          FC_ets <- forecast(ets(train,
                                                                                                                 allow.multiplicative.trend = TRUE,
                                                                                                                 opt.crit = c("lik", "amse", "mse", "sigma", "mae")),
                                                                                                             h = forecast_n)
                                                                                          prediction_ets[m] <- FC_ets[[2]][[forecast_n]]
                                                                                          ets_months[m] <- tsp(train)[2] + ((forecast_n)/12)
                                                                                          m <- m+1
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    
                                                                                    # Convert the month matrix to a dataframe
                                                                                    ets_month_df <- ets_months %>%
                                                                                      as.data.frame()
                                                                                    # Extract first non-NA month
                                                                                    for(i in 1:nrow(ets_month_df)){
                                                                                      if(all(is.na(ets_month_df))){
                                                                                        ets_start <- NA 
                                                                                      } else{
                                                                                        if(is.na(ets_month_df[i,])){
                                                                                          next
                                                                                        } else{
                                                                                          ets_start <- ets_month_df[i,]
                                                                                          break
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    # Convert to yearmon format
                                                                                    ets_start <- as.yearmon(ets_start)
                                                                                    
                                                                                    # Replace negative predictions with 0
                                                                                    prediction_ets <- replace(prediction_ets, prediction_ets < 0, 0) %>% 
                                                                                      round(2) %>%
                                                                                      as.integer() 
                                                                                    
                                                                                    # Converts the predictions into a TS object for plotting and measuring accuracy
                                                                                    ets_FC <- prediction_ets %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                                                                                    
                                                                                    # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                                                                                    my_ets_Accuracy <- ets_FC %>%
                                                                                      accuracy(tsmyY)
                                                                                    
                                                                                    my_ets_Accuracy <- my_ets_Accuracy[,c(2,3,5)] %>% # Subsets RMSE, MAE, and MAPE accuracy measurements
                                                                                      as.data.frame() %>% # Converts accuracy measurements to a dataframe
                                                                                      t() # Transposes the dataset to have RMSE, MAE, and MAPE as the column names
                                                                                    rownames(my_ets_Accuracy) <- NULL # Removes the rownames
                                                                                    
                                                                                    # Add Holdout_Start to the df
                                                                                    my_ets_Accuracy$Holdout_Start <- ets_start
                                                                                    
                                                                                    # Convert back to dataframe
                                                                                    my_ets_Accuracy <- as.data.frame(my_ets_Accuracy)
                                                                                    
                                                                                    # Rename columns
                                                                                    colnames(my_ets_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                                                                                  }
                                                                                  
                                                                                
                                                                                  ### Moving Average Model ###
                                                                                  if(nrow(myY) < 3){
                                                                                    forecast_MA_df <- NULL
                                                                                    my_ma_Accuracy <- NULL
                                                                                  } else{
                                                                                    isolate({
                                                                                      TS_mySeries_MA <- forecast(auto.arima(myY, 
                                                                                                                            max.p=0, 
                                                                                                                            stationary=TRUE, 
                                                                                                                            seasonal=FALSE),
                                                                                                                 h = forecast_n,
                                                                                                                 level = as.numeric(input$conf_int)
                                                                                      )
                                                                                    })
                                                                                    
                                                                                    # Convert elements of time series FORECAST to dataframe for plotting
                                                                                    forecast_MA_df <- with(TS_mySeries_MA,
                                                                                                           data.frame(Mean=TS_mySeries_MA$mean[forecast_n],
                                                                                                                      Upper=TS_mySeries_MA$upper[forecast_n, 1],
                                                                                                                      Lower=TS_mySeries_MA$lower[forecast_n, 1]))
                                                                                    
                                                                                    # Add Date column to the forecasted values data.frame
                                                                                    forecast_MA_df$Date <- as.Date(max(mySeries_filtered$INCMONTH)) %m+% months(forecast_n)
                                                                                    
                                                                                    # Add column for model name
                                                                                    forecast_MA_df$Model <- "MOVING_AVERAGE"
                                                                                    
                                                                                    # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                                                                                    forecast_MA_df <- forecast_MA_df %>%
                                                                                      select(Model, Date, Mean, Upper, Lower) %>%
                                                                                      mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                                                                           0,
                                                                                                           as.integer(round(Mean, 2))),
                                                                                             Upper = ifelse(as.integer(Upper) < 0,
                                                                                                            0,
                                                                                                            as.integer(round(Upper, 2))),
                                                                                             Lower = ifelse(as.integer(Lower) < 0,
                                                                                                            0,
                                                                                                            as.integer(round(Lower, 2))))
                                                                                    
                                                                                    # Recursive forecast for MA model
                                                                                    for (i in holdout_start:holdout_end){
                                                                                      
                                                                                      if(i < 2){
                                                                                        prediction_ma[s] <- NA
                                                                                        ma_months[s] <- NA
                                                                                        s <- s+1
                                                                                      } else{
                                                                                        train <- window(tsmyY, end = timeProp + ((i-1)/12))
                                                                                        
                                                                                        if(nmonths(train) < 3){
                                                                                          prediction_ma[s] <- NA
                                                                                          ma_months[s] <- NA
                                                                                          s <- s+1
                                                                                        } else{
                                                                                          FC_ma <- forecast(auto.arima(train,
                                                                                                                       max.p=0,
                                                                                                                       stationary=TRUE,
                                                                                                                       seasonal=FALSE),
                                                                                                            h = forecast_n)
                                                                                          prediction_ma[s] <- FC_ma[[4]][[forecast_n]]
                                                                                          ma_months[s] <- tsp(train)[2] + ((forecast_n)/12)
                                                                                          s <- s+1
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    
                                                                                    # Convert the month matrix to a dataframe
                                                                                    ma_month_df <- ma_months %>%
                                                                                      as.data.frame()
                                                                                    # Extract first non-NA month
                                                                                    for(i in 1:nrow(ma_month_df)){
                                                                                      if(all(is.na(ma_month_df))){
                                                                                        ma_start <- NA 
                                                                                      } else{
                                                                                        if(is.na(ma_month_df[i,])){
                                                                                          next
                                                                                        } else{
                                                                                          ma_start <- ma_month_df[i,]
                                                                                          break
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    # Convert to yearmon format
                                                                                    ma_start <- as.yearmon(ma_start)
                                                                                    
                                                                                    # Replace negative predictions with 0
                                                                                    prediction_ma <- replace(prediction_ma, prediction_ma < 0, 0) %>% 
                                                                                      round(2) %>%
                                                                                      as.integer()
                                                                                    
                                                                                    # Converts the predictions into a TS object for plotting and measuring accuracy
                                                                                    ma_FC <- prediction_ma %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                                                                                    
                                                                                    # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                                                                                    my_ma_Accuracy <- ma_FC %>%
                                                                                      accuracy(tsmyY)
                                                                                    
                                                                                    my_ma_Accuracy <- my_ma_Accuracy[,c(2,3,5)] %>%
                                                                                      as.data.frame() %>% 
                                                                                      t()
                                                                                    rownames(my_ma_Accuracy) <- NULL
                                                                                    
                                                                                    # Add Holdout_Start to the df
                                                                                    my_ma_Accuracy$Holdout_Start <- ma_start
                                                                                    
                                                                                    # Convert back to dataframe
                                                                                    my_ma_Accuracy <- as.data.frame(my_ma_Accuracy)
                                                                                    
                                                                                    # Rename columns
                                                                                    colnames(my_ma_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                                                                                  }
                                                                                  
                                                                                  ###########################
                                                                                  ### SES ###
                                                                                  if(nrow(myY) < 3){
                                                                                    forecast_SES_df <- NULL
                                                                                    my_ses_Accuracy <- NULL
                                                                                  } else{
                                                                                    isolate({
                                                                                      # Forecast SES model n periods 
                                                                                      TS_mySeries_SES <- forecast::ses(myY,
                                                                                                                       h=forecast_n,
                                                                                                                       initial=c('optimal', 'simple'),
                                                                                                                       level = as.numeric(input$conf_int)
                                                                                      )
                                                                                    })
                                                                                    
                                                                                    # Convert elements of time series FORECAST to dataframe
                                                                                    forecast_SES_df <- with(TS_mySeries_SES,
                                                                                                            data.frame(Mean=TS_mySeries_SES$mean[forecast_n],
                                                                                                                       Upper=TS_mySeries_SES$upper[forecast_n, 1],
                                                                                                                       Lower=TS_mySeries_SES$lower[forecast_n, 1]))
                                                                                    para_SES <- TS_mySeries_SES$model$components
                                                                                    forecast_SES_df$Parameters <- with(forecast_SES_df, paste0("ETS(", para_SES[1], ",", para_SES[2], ",", para_SES[3], ",", para_SES[4], ")"))
                                                                                    
                                                                                    # Add Date column to the forecasted values data.frame
                                                                                    forecast_SES_df$Date <- as.Date(max(mySeries_filtered$INCMONTH)) %m+% months(forecast_n)
                                                                                    
                                                                                    # Add column for model name
                                                                                    forecast_SES_df$Model <- "SES"
                                                                                    
                                                                                    # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                                                                                    forecast_SES_df <- forecast_SES_df %>%
                                                                                      select(Model, Date, Mean, Upper, Lower) %>%
                                                                                      mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                                                                           0,
                                                                                                           as.integer(round(Mean, 2))),
                                                                                             Upper = ifelse(as.integer(Upper) < 0,
                                                                                                            0,
                                                                                                            as.integer(round(Upper, 2))),
                                                                                             Lower = ifelse(as.integer(Lower) < 0,
                                                                                                            0,
                                                                                                            as.integer(round(Lower, 2))))
                                                                                    
                                                                                    # Recursive forecast for SES model
                                                                                    for (i in holdout_start:holdout_end){
                                                                                      
                                                                                      if(i < 2){
                                                                                        prediction_ses[t] <- NA
                                                                                        ses_months[t] <- NA
                                                                                        t <- t+1
                                                                                      } else{
                                                                                        train <- window(tsmyY, end = timeProp + ((i-1)/12))
                                                                                        
                                                                                        if(nmonths(train) < 3){
                                                                                          prediction_ses[t] <- NA
                                                                                          ses_months[t] <- NA
                                                                                          t <- t+1
                                                                                        } else{
                                                                                          FC_ses <- forecast::ses(train,
                                                                                                                  h=forecast_n,
                                                                                                                  initial=c('optimal', 'simple')
                                                                                          )
                                                                                          prediction_ses[t] <- FC_ses[[2]][[forecast_n]]
                                                                                          ses_months[t] <- tsp(train)[2] + ((forecast_n)/12)
                                                                                          t <- t+1
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    
                                                                                    # Convert the month matrix to a dataframe
                                                                                    ses_month_df <- ses_months %>%
                                                                                      as.data.frame()
                                                                                    # Extract first non-NA month
                                                                                    for(i in 1:nrow(ses_month_df)){
                                                                                      if(all(is.na(ses_month_df))){
                                                                                        ses_start <- NA 
                                                                                      } else{
                                                                                        if(is.na(ses_month_df[i,])){
                                                                                          next
                                                                                        } else{
                                                                                          ses_start <- ses_month_df[i,]
                                                                                          break
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    # Convert to yearmon format
                                                                                    ses_start <- as.yearmon(ses_start)
                                                                                    
                                                                                    # Replace negative predictions with 0
                                                                                    prediction_ses <- replace(prediction_ses, prediction_ses < 0, 0) %>% 
                                                                                      round(2) %>%
                                                                                      as.integer() 
                                                                                    
                                                                                    # Converts the predictions into a TS object for measuring accuracy
                                                                                    ses_FC <- prediction_ses %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                                                                                    
                                                                                    # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                                                                                    my_ses_Accuracy <- ses_FC %>%
                                                                                      accuracy(tsmyY)
                                                                                    
                                                                                    my_ses_Accuracy <- my_ses_Accuracy[,c(2,3,5)] %>%
                                                                                      as.data.frame() %>% 
                                                                                      t()
                                                                                    rownames(my_ses_Accuracy) <- NULL
                                                                                    
                                                                                    # Add Holdout_Start to the df
                                                                                    my_ses_Accuracy$Holdout_Start <- ses_start
                                                                                    
                                                                                    # Convert back to dataframe
                                                                                    my_ses_Accuracy <- as.data.frame(my_ses_Accuracy)
                                                                                    
                                                                                    # Rename columns
                                                                                    colnames(my_ses_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                                                                                  }
                                                                                  
                                                                                  
                                                                                  
                                                                                  
                                                                                  isolate({
                                                                                    combined_df <- cbind(rbind(forecast_ARIMA_df,
                                                                                                               forecast_ets_df,
                                                                                                               forecast_MA_df,
                                                                                                               forecast_SES_df),
                                                                                                         rbind(my_arima_Accuracy,
                                                                                                               my_ets_Accuracy,
                                                                                                               my_ma_Accuracy,
                                                                                                               my_ses_Accuracy))
                                                                                    return(combined_df %>%
                                                                                             select(Model, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                                                                                             mutate(RMSE = round(RMSE, 2), MAE = round(MAE, 2), MAPE = round(MAPE, 2), Holdout_Start = as.character(Holdout_Start)) %>%
                                                                                             datatable(rownames = FALSE, options = list(pageLength = 12, dom = "t", ordering = F)) %>%
                                                                                             formatStyle(
                                                                                               'MAPE',
                                                                                               fontWeight = 'bold', # Makes the font for the MAPE column bold
                                                                                               backgroundColor = styleInterval(c(15,45), c('#8DDA77', '#F3EC58', '#E75943')) # Formats the cells based on the value, <15~green, >15&<45~yellow, >45 red
                                                                                             ))
                                                                                  })
                                                                                })
                                                                 })
  
  #############################################
  ############# REGRESSION #################
  ############################################
  
    
    wmj2 <- my_wmj %>% select(Client.Name, Project.Type, Project.Office.Name, Billable.Hours, Month)
    wmj2 <- wmj2 %>% group_by(Month, Project.Type, Client.Name, Project.Office.Name) %>% summarise(Billable.Hours = sum(Billable.Hours, na.rm = TRUE))
    wmj2 <- as.data.frame(wmj2)
    
    dat <- melt(wmj2, id.vars=c("Client.Name", "Project.Type", "Project.Office.Name","Month"))
    dat$Month <- as.integer(factor(dat$Month, levels = month.name))
    
    
    library(plyr)
    models <- dlply(dat, c("Client.Name", "Project.Type", "Project.Office.Name"), function(df) 
      lm(value ~ Month, data = df))
    reg <- ldply(models, coef)
    
    
    detach(package:plyr)
    reg_result <- reg %>% group_by(Client.Name, Project.Type, Project.Office.Name) %>% 
      mutate(Forecast_Regression = (Month * (c(as.numeric(input$reg_months))) + `(Intercept)`))
    reg_result <- reg_result[,c(1,2,3,6)]
    reg_result <- reg_result  %>% group_by(Client.Name, Project.Type, Project.Office.Name) %>% summarise(Forecast_Regression = sum(Forecast_Regression, na.rm = TRUE))
    reg_result <- as.data.frame(reg_result)
    reg_result <- reg_result %>% filter(Forecast_Regression > 0)
  
   
    
    # metrics
    output$billing_regression = renderText({
      round(sum(reg_result$Forecast_Regression),1)
    })
    

    output$billing_regression_table = render_table3(wmj2)

  

  })

}


shinyApp(ui = ui, server = server)
