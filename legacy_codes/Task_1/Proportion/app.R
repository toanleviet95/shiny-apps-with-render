library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Hypothesis Testing for Proportions"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      numericInput(inputId = "sample_size", label = "Choose sample size", value = 500),
      sliderInput(inputId = "p_0", label = "Initial proportion value", value = 0.5, min = 0, max = 1, step = 0.1),
      sliderInput(inputId = "p_hat", label = "Sample proportion value", value = 0.5, min = 0, max = 1, step = 0.1),
      selectInput(inputId = "alternative", label = "Choose alternative hypothesis", choices = c("greater", "less", "two.sided")),
      selectInput(inputId = "signif_level", label = "Significant Level", choices = c(0.01, 0.05, 0.1))
    ),
    
    mainPanel(
      verbatimTextOutput(outputId = "result")
    )
  )
)

server <- function(input, output){
  z.test <- function(n, p0, phat, signif.level, alt){
    if (length(p0) > 0){
      se = sqrt(p0*(1-p0)/n)
      z = (phat - p0)/se
      if (alt == "less"){
        pval = pnorm(z)
      }
      else if (alt == "greater"){
        pval = 1 - pnorm(z)
      }
      else if (alt == "two.sided"){
        pval = 2*pnorm(z)
      }
    }
    cint = c(phat - qnorm(1 - as.numeric(signif.level)/2)*se, phat + qnorm(1 - as.numeric(signif.level)/2)*se)
    return(list(estimate=phat, z=z, pval=pval, cint=cint))
  }
  
  output$result <- renderPrint({
    sf = as.numeric(input$signif_level)
    conf_level = 1 - sf
    if (is.numeric(input$p_0) == TRUE && is.numeric(input$p_hat) == TRUE){
      test = z.test(n = input$sample_size, p0 = input$p_0, phat = input$p_hat, signif.level = input$signif_level, alt = input$alternative)
      print("1-sample proportions test")
      print(paste("Null probability:",input$p_0))
      print(paste("p-value:",round(test$pval,4)))
      if (input$alternative == "less"){
        print(paste("Alternative hypothesis: true p is less than",input$p_0))
      }
      else if (input$alternative == "greater"){
        print(paste("Alternative hypothesis: true p is greater than",input$p_0))
      }
      else if (input$alternative == "two.sided"){
        print(paste("Alternative hypothesis: true p is not equal to",input$p_0))
      }
      print(paste(100*conf_level,"percent confidence interval:"))
      print(paste(round(test$cint,4)))
      print(paste("sample estimates: ",round(test$estimate,4)))
    }
  })
  
}

shinyApp(ui = ui, server = server)