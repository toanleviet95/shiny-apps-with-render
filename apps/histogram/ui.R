ui <- fluidPage(

  # App title ----
  titlePanel("Histogram"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(
        inputId = "bins",
        label = "Number of bins:",
        min = 1,
        max = 50,
        value = 10
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  )
)
