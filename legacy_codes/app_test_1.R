library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel(title = "Use of action/submit button for multiple inputs"), 
  sidebarLayout(
    
    sidebarPanel(numericInput("rows","Input No. of rows",value = 3,min=1),
                 br(),
                 numericInput("col","Input No. of cols",value = 1,min=1),
                 actionButton('update' ,'update!')),
    
    mainPanel(textOutput("display"),
              uiOutput("plo")
              
    )))) 

server <- function(input,output){
  
  # creating input widgets dynamically
  output$plo <- renderUI({
    z <- input$col
    
    lapply(seq(input$col), function(j){
      column(width=3,
             lapply(seq(input$rows),function(i){
               numericInput(inputId = paste0("range",paste0(i,j)),label = j,value = paste0(i,j))  
             })
      )
    })
  })
  
  # capturing the value of input widgets in a matrix
  cm <-  reactive({
    c <- input$col
    r <- input$rows
    
    changed_m <- matrix(nrow = r,ncol = c)
    lapply(seq(input$col), function(j){
      lapply(seq(input$rows),function(i){
        x=input[[paste0("range",paste0(i,j))]]
        changed_m[i,j] <<- ifelse(!is.null(x),x,0)
      })
    })
    changed_m
  }) 
  
  # initialize our reactiveVal with an empty string
  my_sum <- reactiveVal('')
  
  # observer that listens to the button click, then updates the sum string.
  observeEvent(input$update,{
    my_sum(paste0("Sum of matrix:   ",sum(cm())))
  })
  
  # observer that listens to changes in the input, then updates the sum string.
  observeEvent(cm(),ignoreNULL = T,ignoreInit = T, {
    isolate(my_sum('invalidated. Press button to update.'))
  })
  
  # display the sum string
  output$display <- renderText({
    my_sum()
  })
} 

shinyApp(ui,server)
