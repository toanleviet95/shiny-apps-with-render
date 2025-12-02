library(shiny)
library(DT)

shinyUI(fluidPage(
  # Application title
  titlePanel("Statistical Inference for Categorical Data"),
  withMathJax(),
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
                             br(),
                             h4("Step 2: What you want to do?"),
                             radioButtons("type_jobs", "Select one option",
                                          choices = c("Nothing" = "non", "Goodness-of-fit Test" = "GF",
                                                      "Chi-square Test for Independence" = "Ind"),
                                          selected = "non")# ,
                             # actionButton("butt_select", "Select")
                ),
                ##
                mainPanel(
                  tabsetPanel(id = "out1",
                              tabPanel(title = "Data", dataTableOutput("table_data")),
                              tabPanel(title = "Goodness-of-fit Test",
                                       mainPanel(
                                         br(),
                                         uiOutput("helptext_1"),
                                         uiOutput("p_0"),
                                         br(),
                                         uiOutput("hypothesis_1_1"),
                                         uiOutput("hypothesis_1_2"),
                                         br(),
                                         uiOutput("signf_level_1"),
                                         uiOutput("butt_run_test_1"),
                                         br(),
                                         uiOutput("show_res_exp_1"),
                                         tableOutput("table_expection_1"),
                                         uiOutput("show_res_test_1"),
                                         tableOutput("res_test_1"),
                                         uiOutput("conclusion_1"),
                                         br()
                                         )
                              ),
                              tabPanel(title = "Chi-square Test for Independence",
                                       br(),
                                       uiOutput("hypothesis_2_1"),
                                       uiOutput("hypothesis_2_2"),
                                       br(),
                                       uiOutput("signf_level_2"),
                                       uiOutput("butt_run_test_2"),
                                       br(),
                                       uiOutput("show_plot_mosaic"),
                                       plotOutput("plot_mosaic"),
                                       uiOutput("show_res_test_2"),
                                       tableOutput("res_test_2"),
                                       uiOutput("conclusion_2"),
                                       br()
                              )
                  )
                )
  )
)
)
