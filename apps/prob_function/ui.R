ui <- fluidPage(

  # App title ----
  titlePanel("Probability distribution of Binomial random variable"),
  withMathJax(),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "num_obs", label = "Number of trials", value = 1, min = 1, step = 1),
      numericInput(
        inputId = "prob", label = "Probability of success", value = 0.5, min = 0, max = 1,
        step = 0.05
      ),
      br(),
      checkboxInput("compute", "Calculate the probability", FALSE),
      uiOutput("model_prob"),
      uiOutput("val_a"),
      uiOutput("val_b")
      # sliderInput(inputId = "bins",
      #             label = "Number of possible values:",
      #             min = 1,
      #             max = 100,
      #             value = 1)
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      downloadButton("downloadPlot", "Download plot"),
      br(),
      br(),
      br(),
      textOutput("result"),
      tableOutput(outputId = "summary"),
      uiOutput(outputId = "calculator")
    )
  )
)
