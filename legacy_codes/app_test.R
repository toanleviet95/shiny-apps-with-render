library(shiny)

ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  mainPanel(
    numericInput("mean", label = "mean", value = 1),
    withMathJax(tableOutput("table"))
  )
)

server <- function(input, output) {
  
  output$table <- renderTable({
    x <- rnorm(2)
    y <- rnorm(2, input$mean)
    tab <- data.frame(x = x, y = y)
    rownames(tab) <- c("\\(\\alpha\\)", 
                       "\\(\\beta\\)")
    tab
  },
  include.rownames = TRUE,
  include.colnames = TRUE)
  
}

shinyApp(ui, server)
