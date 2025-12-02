library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Normal density curve"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      #Using sliderInput
      sliderInput(inputId = "mean_val", label = "Mean", value = 0, min = -5, max = 5),
      sliderInput(inputId = "sd_val", label = "Standard deviation", value = 1, min = 1, max = 5, step = 0.1)
    ),
    
    mainPanel(
      plotOutput(outputId = "plot")
)
)
)

server <- function(input, output){
  output$plot <- renderPlot({
    x <- seq(-20, 20, by = 0.001)
    y <- dnorm(x, mean = input$mean_val, sd = input$sd_val)
    plot(x, y, main = paste("Normal density curve"), ylab = "", xlab = "", type = "l", col = "blue",
         ylim = c(0, 0.4), lwd = 1.5)
    grid()
    mtext(bquote(mu == .(input$mean_val) ~ "," ~ sigma == .(input$sd_val)))
  })
}

shinyApp(ui = ui, server = server)