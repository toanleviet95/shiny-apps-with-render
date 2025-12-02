ui <- fluidPage(
  # Application title
  titlePanel("Statistical Inference for Population Mean"),
  withMathJax(),
  sidebarLayout(
    position = "left",
    # Sidebar with a slider input
    sidebarPanel(
      style = "overflow-y:scroll; max-height: 850px; position:relative;",
      tags$head(
        tags$style(type = "text/css", "select { max-width: 400px; }"),
        tags$style(type = "text/css", ".span4 { max-width: 400px; }"),
        tags$style(type = "text/css", ".well { max-width: 400px; }")
      ),
      h3("Steps to analysis"),
      h4("Step 1: Load the data"),
      fileInput(
        inputId = "data_file", label = "Choose data file",
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      helpText("The data file should be .csv, .txt or .dat file!"),
      # tags$hr(), #Adds a horizontal line
      checkboxInput(inputId = "header", label = "Header", value = TRUE),
      radioButtons(
        inputId = "sep", label = "Separator",
        choices = c("Space" = "", "Tab" = "\t", "Comma" = ",", "Semicolon" = ";"),
        selected = ","
      ),
      radioButtons(
        inputId = "quote", label = "Quote",
        choices = c("None" = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'
      ),
      br(),
      h4("Step 2: What you want to do?"),
      radioButtons("type_jobs", "Select one option",
        choices = c(
          "Noting" = "non", "Confidence interval" = "CI",
          "One-sample testing" = "one",
          "Two-sample testing" = "two"
        ),
        selected = "non"
      ) # ,
      # actionButton("butt_select", "Select")
    ),
    ##
    mainPanel(
      tabsetPanel(
        id = "out1",
        tabPanel(title = "Data", dataTableOutput("table_data")),
        tabPanel(
          title = "Confidence Interval",
          mainPanel(
            br(),
            uiOutput("var_ci"),
            uiOutput("ci_level"),
            uiOutput("butt_run_ci"),
            br(),
            uiOutput(outputId = "ci")
          )
        ),
        tabPanel(
          title = "One-sample testing",
          br(),
          uiOutput("var_test_1"),
          uiOutput("mean_0"),
          uiOutput("hypothesis"),
          uiOutput("signf_level"),
          uiOutput("butt_run_test_1"),
          br(),
          tableOutput("res_test_1"),
          uiOutput("conclusion_1")
        ),
        tabPanel(
          title = "Two-sample testing",
          br(),
          uiOutput("var_test_2_1"),
          uiOutput("var_test_2_2"),
          uiOutput("hypothesis_2"),
          uiOutput("check_paired"),
          uiOutput("check_variance"),
          uiOutput("signf_level_2"),
          uiOutput("butt_run_test_2"),
          br(),
          tableOutput("res_test_2"),
          uiOutput("conclusion_2")
        )
      )
    )
  )
)
