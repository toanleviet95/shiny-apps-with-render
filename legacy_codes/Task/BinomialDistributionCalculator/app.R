library(shiny)
library(formattable)

ui <- fluidPage(
  
  # Application title
  titlePanel("Binomial Distribution Calculator"),
  withMathJax(),
  sidebarLayout(position = "left",
    
    # Sidebar with a slider input
    sidebarPanel(
      numericInput(inputId = "num_obs", label = "Number of trials", value = NULL, min = 1),
      numericInput(inputId = "prob", label = "Probability of success", value = NULL, min = 0, max = 1),
      radioButtons(inputId = "model", label = "Model of probability",
                   choiceNames = list("Pr(X = a)", "Pr(X < a)", "Pr(X \u2264 a)", "Pr(X > a)", 
                                      "Pr(X \u2265 a)", "Pr(a < X < b)", paste(bquote("Pr(a \u2264 X"), bquote("b)")),
                                      "Pr(a < X \u2264 b)",  "Pr(a \u2264 X \u2264 b)"),
                   choiceValues = c(1, 2, 3, 4, 5, 6, 7, 8, 9))
    ),
    mainPanel(
      uiOutput(outputId = "val_1"),
      uiOutput(outputId = "val_2"),
      uiOutput(outputId = "calculator")
    )
  )
)

server <- function(input, output, session){
  output$val_1 <- renderUI({
    numericInput(inputId = "value_1", label = "Input value of a", value = NULL, min = 0)
  })
  
  output$val_2 <- renderUI({
    if (input$model == 6 | input$model == 7 | input$model == 8 | input$model == 8)
      numericInput(inputId = "value_2", label = "Input value of b", value = NULL, min = 0)
  })
  
  output$calculator <- renderUI({
    n = input$num_obs
    p = input$prob
    a = input$value_1
    # b = input$value_2
    
    if (a <= input$num_obs){
      if (input$model == "Pr(X < a)"){
        # invalidateLater(5000, session)
        result = round(pbinom(a - 1, size = n, prob = p), 4) #P(X<a)
        withMathJax(sprintf("The probability is:
                        $$Pr(X \\leq %d ) = %.03f$$", a, result))
        #paste("Pr( X < ",a,") =",result)
        
        # else if (input$type == "Upper Tail"){
        #   result = 1 - round(pbinom(a, size = n, prob = p),4) #1 - P(X<=a)
        #   paste("Pr( X >",a,") =",result)
        # }
        # else if (input$type == "Middle"){
        #   if (is.numeric(b) == TRUE){
        #     if (b > a){
        #       result = round(pbinom(b - 1, size = n, prob = p) - pbinom(a, size = n, prob = p),4) #P(X<b) - P(X<=a)
        #       paste("Pr(",a,"< X <",b,") =",result) 
        #     }
        #     else 
        #       print("Please check your value!")
        #   }
        # }
        
      } 
      # else if (input$model == "Pr(X \u2264 a)"){
      #   result = round(pbinom(a, size = n, prob = p),4) #P(X<=a)
      #   #paste(bquote("P( X \u2264"),a,") =",result)
      #   paste("Pr( X <=",a,") =",result)
      #   
      #   # else if (input$type == "Upper Tail"){
      #   #   result = 1 - round(pbinom(1 - a, size = n, prob = p),4) # 1 - P(X<a)
      #   #   #paste(bquote("P( X \u2265"),a,") =",result)
      #   #   paste("Pr( X >=",a,") =",result)
      #   # }
      #   # else if (input$type == "Middle"){
      #   #   if (is.numeric(b) == TRUE){
      #   #     if (b > a){
      #   #       result = round(pbinom(b, size = n, prob = p) - pbinom(1 - a, size = n, prob = p),4) #P(X<=b) - P(X<a)
      #   #       #paste("P(",a,bquote("\u2264 X \u2264"),b,") =",result)
      #   #       paste("Pr(",a,"<=  X <=",b,") =",result)
      #   #     }
      #   #     else 
      #   #       print("Please check your value!")
      #   #   }
      #   # }
      # }
    }
    
  })
}

shinyApp(ui = ui, server = server)