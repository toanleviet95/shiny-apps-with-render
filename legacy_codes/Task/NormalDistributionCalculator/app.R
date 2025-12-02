library(shiny)


ui <- fluidPage(
  
  # Application title
  titlePanel("Probability distribution of Normal random variable"),
  withMathJax(),
  sidebarLayout(
    # Sidebar with a slider input
    sidebarPanel(
      #Using numericInput
      numericInput(inputId = "mean_val", label = "Mean", value = 0),
      numericInput(inputId = "sd_val", label = "Standard deviation", value = 1),
      radioButtons(inputId = "type", label = "Model of probability",
                   choiceNames = list("Pr(X \u2264 a)", "Pr(X \u2265 a)", "Pr(a \u2264 X \u2264 b)"),
                   choiceValues = c(1, 2, 3))
      #selectInput(inputId = "type", label = "Type", choices = c("Lower Tail", "Upper Tail", "Middle"))
    ),
    mainPanel(
      uiOutput(outputId = "val_1"),
      uiOutput(outputId = "val_2"),
      plotOutput(outputId = "plot"),
      uiOutput(outputId = "calculator")
    )
  )
)

server <- function(input, output){
  output$val_1 <- renderUI({
    numericInput(inputId = "value_1", label = "Input value of a", value = 0)
  })
  #
  output$val_2 <- renderUI({
    if (input$type == 3)
      numericInput(inputId = "value_2", label = "Input value of b", value = 1)
  })
  #
  output$plot <- renderPlot({
    if(input$type == 1){
      if(is.null(input$mean_val)) return(NULL)
      if(is.null(input$sd_val)) return(NULL)
      if(is.null(input$value_1)) return(NULL)
      isolate({
        cord.x <- c(input$value_1, seq(input$mean_val - 4*input$sd_val, input$value_1, 0.01), input$value_1) 
        cord.y <- c(0, dnorm(seq(input$mean_val - 4*input$sd_val, input$value_1, 0.01), 
                             input$mean_val, input$sd_val), 0) 
        curve(dnorm(x, input$mean_val, input$sd_val), 
              xlim = c(input$mean_val - 4*input$sd_val, input$mean_val + 4*input$sd_val), 
              axes = FALSE, ylab = "", xlab = "", col = "red") 
        polygon(cord.x, cord.y, col = 'skyblue', border = "white")
        axis(side = 1, at = input$mean_val, labels = input$mean_val, tick = FALSE)
        axis(side = 1, at = input$value_1, labels = input$value_1, tick = FALSE)
        abline(v = input$mean_val, lty = 2, col = "gray60")
        abline(h = 0)
      })
    }
    if(input$type == 2){
      if(is.null(input$mean_val)) return(NULL)
      if(is.null(input$sd_val)) return(NULL)
      if(is.null(input$value_1)) return(NULL)
      isolate({
        cord.x <- c(input$value_1, seq(input$value_1, input$mean_val + 4*input$sd_val, 0.01), 
                    input$mean_val + 4*input$sd_val) 
        cord.y <- c(0, dnorm(seq(input$value_1, input$mean_val + 4*input$sd_val, 0.01), 
                             input$mean_val, input$sd_val), 0) 
        curve(dnorm(x, input$mean_val, input$sd_val), 
              xlim = c(input$mean_val - 4*input$sd_val, input$mean_val + 4*input$sd_val), 
              axes = FALSE, ylab = "", xlab = "", col = "red") 
        polygon(cord.x, cord.y, col = 'skyblue', border = "white")
        axis(side = 1, at = input$mean_val, labels = input$mean_val, tick = FALSE)
        axis(side = 1, at = input$value_1, labels = input$value_1, tick = FALSE)
        abline(v = input$mean_val, lty = 2, col = "gray60")
        abline(h = 0)
      })
    }
    if(input$type == 3){
      if(is.null(input$mean_val)) return(NULL)
      if(is.null(input$sd_val)) return(NULL)
      if(is.null(input$value_1)) return(NULL)
      if(is.null(input$value_2)) return(NULL)
      isolate({
        cord.x <- c(input$value_1, seq(input$value_1, input$value_2, 0.01), input$value_2) 
        cord.y <- c(0, dnorm(seq(input$value_1, input$value_2, 0.01), input$mean_val, input$sd_val), 0) 
        curve(dnorm(x, input$mean_val, input$sd_val), 
              xlim = c(input$mean_val - 4*input$sd_val, input$mean_val + 4*input$sd_val), 
              axes = FALSE, ylab = "", xlab = "", col = "red") 
        polygon(cord.x, cord.y, col = 'skyblue', border = "white")
        axis(side = 1, at = input$mean_val, labels = input$mean_val, tick = FALSE)
        axis(side = 1, at = input$value_1, labels = input$value_1, tick = FALSE)
        axis(side = 1, at = input$value_2, labels = input$value_2, tick = FALSE)
        abline(v = input$mean_val, lty = 2, col = "gray60")
        abline(h = 0)
        })
    }
  })
  #
  output$calculator <- renderUI({
    m = as.numeric(input$mean_val)
    sd = as.numeric(input$sd_val)
    a = input$value_1
    b = input$value_2
    
    if (is.numeric(a) == TRUE){
      if (input$type == 1){
        result = round(pnorm(a, mean = m, sd = sd), 4)
        withMathJax(sprintf("The probability is:
                        $$Pr(X \\leq %0.03f) = %.03f$$", a, result))
        # paste("P(X <",a,") =",result)
      } 
      else if (input$type == 2){
        result = 1 - round(pnorm(a, mean = m, sd = sd),4)
        withMathJax(sprintf("The probability is:
                        $$Pr(X \\geq %0.03f) = %.03f$$", a, result))
        #paste("P(X >",a,") =",result)
      }
      else if (input$type == 3){
        if (is.numeric(b) == TRUE){
          if (b > a){
            result = round(pnorm(b, mean = m, sd = sd) - pnorm(a, mean = m, sd = sd), 4) 
            withMathJax(sprintf("The probability is:
                        $$Pr(%0.03f \\leq X \\leq %0.03f) = %.03f$$", a, b, result))
            # paste("P(",a,"< X <",b,") =",result) 
          }
          else 
            print("Please check your value!")
        }
      }
    }
  })
  #
}

shinyApp(ui = ui, server = server)