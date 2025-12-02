library(shiny)
shinyServer(function(input, output) {
  output$model_prob <- renderUI({
    if(input$compute){
      radioButtons(inputId = "model", label = "Model of probability",
                   choiceNames = list(HTML("<p style='color:red;'>Pr(X = a)</p>"), 
                                      HTML("<p style='color:red;'>Pr(X \u2264 a)</p>"), 
                                      HTML("<p style='color:red;'>Pr(X \u2265 a)</p>"),
                                      HTML("<p style='color:red;'>Pr(a \u2264 X \u2264 b)</p>")),
                   choiceValues = list("md1", "md2", "md3", "md4"))
    }
  })
  #
  output$val_a <- renderUI({
    if(input$compute){
      numericInput(inputId = "value_1", label = "Input value of a", value = 0, min = 0)
    }
  })
  #
  output$val_b <- renderUI({
    if(input$compute){
      if(input$model == "md4"){
        numericInput(inputId = "value_2", label = "Input value of b", value = 1, min = 1)
      }
    }
  })
  #
  plotInput <- function(){
    if(!input$compute){
      if(is.null(input$value_1) | is.null(input$value_2)){
        y <- 0:input$num_obs
        names(y) <- y
        barplot(dbinom(y, input$num_obs, input$prob), ylab = "Pr(X = x)", xlab = "x", space = 0.5)
      }
    } else{
      y <- 0:input$num_obs
      names(y) <- y
      col_bar <- rep("grey", input$num_obs + 1)
      switch(input$model,
             md1 = col_bar[input$value_1 + 1] <- "forestgreen",
             md2 = col_bar[0:(input$value_1 + 1)] <- "forestgreen",
             md3 = col_bar[(input$value_1 + 1):(input$num_obs + 1)] <- "forestgreen",
             md4 = col_bar[(input$value_1 + 1):(input$value_2 + 1)] <- "forestgreen"
      )
      barplot(dbinom(y, input$num_obs, input$prob), ylab = "Pr(X = x)", xlab = "x", space = 0.5,
              col = col_bar)
    }
  }
  #
  output$distPlot <- renderPlot({
    print(plotInput())
    # if(!input$compute){
    #   y <- 0:input$num_obs
    #   names(y) <- y
    #   barplot(dbinom(y, input$num_obs, input$prob), ylab = "Pr(X = x)", xlab = "x", space = 0.5)
    # } else{
    #   y <- 0:input$num_obs
    #   names(y) <- y
    #   col_bar <- rep("grey", input$num_obs + 1)
    #   switch(input$model, 
    #          md1 = col_bar[input$value_1 + 1] <- "forestgreen",
    #          md2 = col_bar[0:(input$value_1 + 1)] <- "forestgreen",
    #          md3 = col_bar[(input$value_1 + 1):(input$num_obs + 1)] <- "forestgreen",
    #          md4 = col_bar[(input$value_1 + 1):(input$value_2 + 1)] <- "forestgreen"
    #   )
    #   barplot(dbinom(y, input$num_obs, input$prob), ylab = "Pr(X = x)", xlab = "x", space = 0.5, 
    #           col = col_bar)
    # }
    #plot(y, dbinom(y, input$bins, 0.5), type = "h", lwd = 4, ylab = "Pr(X = x)", xlab = "x", xlim = c(0, 50))
  })
  #
  output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("plot-binomial-dist.png")
      },
      content = function(file) {
        png(file)
        plotInput()
        dev.off()
      }
    )
  #
  output$result <- renderText({
    if (is.null(input$num_obs)) return(NULL)
    if (is.null(input$prob)) return(NULL)
    isolate({
      paste("Summary Statistics")
    })
  })
  #
  output$summary <- renderTable({
    if (is.null(input$num_obs)) return(NULL)
    if (is.null(input$prob)) return(NULL)
    isolate({
      u <- c(input$num_obs*input$prob, input$num_obs*input$prob*(1 - input$prob),
             sqrt(input$num_obs*input$prob*(1 - input$prob)))
      tab <- cbind(u)
      rownames(tab) <- c("Mean", "Variance", "Standard deviation")
      colnames(tab) <- c("Value")
    })
    as.data.frame(tab)
  }, digits = 3, bordered = FALSE, width = "75%", align = NULL, rownames = TRUE)
  #
  output$calculator <- renderUI({
    #if(is.null(input$method)) return(NULL)
    #isolate({
    if(input$compute){
      n = input$num_obs
      p = input$prob
      a = input$value_1
      b = input$value_2
      print(input$model)
      print(input$num_obs)
      if (a <= input$num_obs){
        if (input$model == "md1"){
          # invalidateLater(5000, session)
          result = round(dbinom(a, size = n, prob = p), 4) #P(X = a)
          withMathJax(sprintf("The probability is :
                        $$Pr(X = %d) = %.03f$$", a, result))
        }
        else if (input$model == "md2"){
          # invalidateLater(5000, session)
          result = round(pbinom(a, size = n, prob = p), 4) #P(X <= a)
          withMathJax(sprintf("The probability is:
                        $$Pr(X \\leq %d) = %.03f$$", a, result))
        }
        else if (input$model == "md3"){
          # invalidateLater(5000, session)
          result = round(1 - pbinom(a - 1, size = n, prob = p), 4) #P(X >= a)
          withMathJax(sprintf("The probability is:
                        $$Pr(X \\geq %d) = %.03f$$", a, result))
        }
        else{
          # invalidateLater(5000, session)
          if(a <= b){
            result = round(pbinom(b, size = n, prob = p) - pbinom(a - 1, size = n, prob = p), 4) #P(a <= X <= b)
            withMathJax(sprintf("The probability is:
                        $$Pr(%d \\leq X \\leq %d) = %.03f$$", a, b, result))
          }
        }
      }
    }
    #})
  })
})
