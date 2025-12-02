library(shiny)
library(DT)
library(plyr)

ui <- fluidPage(
  
  # Application title
  titlePanel("Lecture 1"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      fileInput(inputId = "file", label = "Choose your CSV file"),
      tags$hr(), #Adds a horizontal line 
      checkboxInput(inputId = "header", label = "Header", value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Data", dataTableOutput(outputId = "contents")),
        tabPanel(title = "Summary", 
                 uiOutput(outputId = "vars"),
                 textOutput("result"),
                 verbatimTextOutput(outputId = "summary"),
                 plotOutput(outputId = "plot"))
      )
    )
  )
)

server <- function(input, output){
  dataInput <- reactive({
    inFile <- input$file
    if (is.null(inFile))  return(NULL)
    read.csv(file = inFile$datapath, header = input$header)
  })
  
  output$contents <- renderDataTable({
    if(is.null(dataInput())) return(NULL)
    dataInput()
  }) 
  
  output$vars <- renderUI({
    if (is.null(dataInput())) return(NULL)
    selectInput(inputId = "selected_var", label = "Select variable", choices = as.list(names(dataInput())),
                selected = as.list(names(dataInput()))[[2]])
  })
  
  output$result <- renderText(paste("Summary statistics of ", input$selected_var))
  
  output$summary <- renderPrint({
    #Numeric
    if(is.numeric(dataInput()[,input$selected_var]) == TRUE)
    {
      if(all(dataInput()[,input$selected_var] %in% 0:1) == FALSE) {summary(dataInput()[,input$selected_var])}
      else {count(dataInput()[,input$selected_var])} #Binary 
    }
    #Categorical
    else
      summary(dataInput()[,input$selected_var])
  })
  
  output$plot <- renderPlot({
    #Numeric
    if(is.numeric(dataInput()[,input$selected_var]) == TRUE)
    {
      if(all(dataInput()[,input$selected_var] %in% 0:1) == FALSE) 
        boxplot(dataInput()[,input$selected_var], main = paste("Box plot of", input$selected_var))
      else 
        barplot(table(dataInput()[,input$selected_var]), main = paste("Bar chart of", input$selected_var)) #Binary 
    }
    #Categorical
    else 
      barplot(table(dataInput()[,input$selected_var]), main = paste("Bar chart of", input$selected_var))
  })
}

shinyApp(ui = ui, server = server)