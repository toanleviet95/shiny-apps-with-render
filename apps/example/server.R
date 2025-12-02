server <- function(input, output, session) {

  # Reactive values
  values <- reactiveValues()

  # Observe button click
  observeEvent(input$submit_btn, {
    values$text <- input$text_input
    values$number <- input$numeric_input
  })

  # Text output
  output$text_output <- renderText({
    if (is.null(values$text)) {
      return("Click submit to see text output")
    }
    paste("You entered:", values$text)
  })

  # Numeric output
  output$numeric_output <- renderText({
    if (is.null(values$number)) {
      return("Click submit to see numeric output")
    }
    paste("Number:", values$number, "\nSquared:", values$number^2)
  })

  # Example plot
  output$example_plot <- renderPlot({
    if (is.null(values$number)) {
      plot(1:10, 1:10, main = "Default Plot", type = "l")
    } else {
      x <- 1:values$number
      y <- x^2
      plot(x, y, main = paste("Plot with n =", values$number),
           type = "b", col = "blue", pch = 16)
    }
  })
}