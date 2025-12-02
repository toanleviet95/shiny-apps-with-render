ui <- fluidPage(
  # Application title
  titlePanel("Statistical Inference for Population Proportion"),
  # withMathJax(),
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
      h4("Step 1: Input the information"),
      numericInput(
        inputId = "num_trails", label = "Number of sample size of trials:", value = 1,
        min = 1
      ),
      numericInput(
        inputId = "p_hat", label = "The sample proportion", value = 0.5, min = 0,
        max = 1
      ),
      br(),
      h4("Step 2: What you want to do?"),
      radioButtons("type_jobs", "Select one option",
        choices = c(
          "Confidence interval" = "CI",
          "Hypothesis testing" = "HT"
        ),
        selected = "CI"
      ) # ,
      # actionButton("butt_select", "Select")
    ),
    ##
    mainPanel(
      tabsetPanel(
        id = "out1",
        tabPanel(
          title = "Confidence Interval",
          mainPanel(
            br(),
            uiOutput("ci_level"),
            uiOutput("butt_run_ci"),
            uiOutput(outputId = "ci")
          )
        ),
        tabPanel(
          title = "Hypothesis testing",
          br(),
          uiOutput("p_0"),
          uiOutput("hypothesis"),
          uiOutput("signf_level"),
          uiOutput("butt_run_test"),
          uiOutput("tableUI"),
          uiOutput("conclusion")
          # withMathJax(tableOutput("res_test"))
        )
      )
    )
  )
)
