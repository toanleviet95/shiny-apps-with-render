library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Descriptive Statistics"),
  sidebarLayout(position = "left",
                # Sidebar with a slider input
                sidebarPanel(style = "overflow-y:scroll; max-height: 850px; position:relative;",
                             tags$head(
                               tags$style(type="text/css", "select { max-width: 400px; }"),
                               tags$style(type="text/css", ".span4 { max-width: 400px; }"),
                               tags$style(type="text/css", ".well { max-width: 400px; }")
                             ),
                             h3("Steps to analysis"),
                             h4("Step 1: Load the data"),
                             fileInput(inputId = "data_file", label = "Choose data file",
                                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                             helpText("The data file should be .csv, .txt or .dat file!"),
                             # tags$hr(), #Adds a horizontal line 
                             checkboxInput(inputId = "header", label = "Header", value = TRUE),
                             radioButtons(inputId = "sep", label = "Separator",
                                          choices = c("Space" = "", "Tab" = "\t", "Comma" = ",", "Semicolon" = ";"),
                                          selected = ","),
                             radioButtons(inputId = "quote", label = "Quote",
                                          choices = c("None" = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'),
                             br()
                             # h4("Step 2: What you want to do?"),
                             # radioButtons("type_jobs", "Select one option",
                             #              choices = c("Confidence interval" = "CI", 
                             #                          "One-sample testing" = "one", 
                             #                          "Two-sample testing" = "two"), 
                             #              selected = "CI"),
                             # actionButton("butt_select", "Select")
                             # uiOutput("vars"),
                             # br(),
                             # uiOutput("type_varible"),
                             # actionButton("butt_sum_data", "Summary data"),
                             # br(),
                             # h4("Step 3: Grapical presentation"),
                             # radioButtons("type_plot", "Choose plot",
                             #              choices = c("Bar plot" = "bar_plot", "Histogram" = "hist", "Boxplot" = "boxpl",
                             #                          "Histogram and Boxplot" = "hist_box"), selected = NULL),
                             # actionButton("butt_run_plot", "Run Plot!")
                ),
                ##
                mainPanel(
                  tabsetPanel(id = "out1",
                              tabPanel(title = "Data", dataTableOutput("table_data")),
                              tabPanel(title = "Confidence Interval",
                                       mainPanel(
                                         br(),
                                         textOutput("result"),
                                         tableOutput(outputId = "summary"))
                              ),
                              tabPanel(title = "One-sample testing",
                                       plotOutput(outputId = "plot"),
                                       downloadButton('downloadPlot', 'Download plot')
                              ),
                              tabPanel(title = "Two-sample testing",
                                       plotOutput(outputId = "plot"),
                                       downloadButton('downloadPlot', 'Download plot')
                              )
                  )
                )
  )
))

