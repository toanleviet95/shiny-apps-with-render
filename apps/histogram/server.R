server <- function(input, output) {
  # Histogram of a random varible ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    # set.seed(101)
    x <- rnorm(150, 2, 4)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x,
      breaks = bins, col = "#75AADB", border = "white",
      xlab = "Random varible X",
      main = "Histogram of X"
    )
  })
}
