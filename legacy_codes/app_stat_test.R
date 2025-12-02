library(shiny)
library(DT)
library(plyr)

ui <- fluidPage(
  
  # Application title
  titlePanel("Hypothesis Testing"),
  
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
        tabPanel(title = "Hypothesis Testing", 
                 uiOutput(outputId = "vars"),
                 uiOutput(outputId = "type"),
                 uiOutput(outputId = "m0"),
                 uiOutput(outputId = "p0"),
                 uiOutput(outputId = "alt"),
                 uiOutput(outputId = "conf"),
                 verbatimTextOutput(outputId = "result")
                 )
      )
    )
  )
)

server <- function(input, output){
  data <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    read.csv(file = inFile$datapath, header = input$header)
  })
  
  output$contents <- renderDataTable({
    if(is.null(data()))
      return(NULL)
    data()
  }) 
  
  output$vars <- renderUI({
    df <- data()
    selectInput(inputId = "selected_var", label = "Select variable", choices = colnames(df[,c(-1)]))
  })
  
  output$type <- renderUI({
    selectInput(inputId = "type_test", label = "Choose type of hypothesis test", choices = c("Mean", "Proportion"))
  })
  
  output$m0 <- renderUI({
    if (is.null(input$type_test) == FALSE){
      if (input$type_test == "Mean"){
        numericInput(inputId = "mean", label = "Choose mean value", value = 0)
      }
    }
  })
  
  output$p0 <- renderUI({
    if (is.null(input$type_test) == FALSE){
      if (input$type_test == "Proportion"){
        sliderInput(inputId = "prop", label = "Choose proportion value", value = 0.5, min = 0, max = 1, step = 0.1)
      }
    }
  })
  
  output$alt <- renderUI({
    selectInput(inputId = "alternative", label = "Choose alternative hypothesis", choices = c("greater", "less", "two.sided"))
  })
  
  output$conf <- renderUI({
    sliderInput(inputId = "conf_level", label = "Confidence Level", value = 0.95, min = 0, max = 1, step = 0.01)
  })
  
  z.test <- function(x, n, p, conf.level, alternative){
    p.hat = x/n
    signif.level = 1 - conf.level
    if (length(p) > 0){
      se = sqrt(p*(1-p)/n)
      z = (p.hat - p)/se
      if (alternative == "less"){
        pval = pnorm(z)
        cint = c(p.hat - pnorm(conf.level))
      }
      else if (alternative == "greater"){
        pval = 1 - pnorm(z)
      }
      else if (alternative == "two.sided"){
        pval = 2*pnorm(z)
      }
    }
    cint = c(p.hat - qnorm(1 - signif.level/2)*se, p.hat + qnorm(1-signif.level/2)*se)
    return(list(estimate=p.hat, z=z, pval=pval, cint=cint))
  }
  
  output$result <- renderPrint({
    temp = input$selected_var
    df = data()
    if (is.null(input$type_test) == FALSE){
      if (input$type_test == "Mean"){
        if (is.numeric(input$mean) == TRUE){
          t.test(df[,temp], mu = input$mean, alternative = input$alternative, conf.level = input$conf_level)
        }
      }
      else if (input$type_test == "Proportion"){
        if (is.numeric(input$prop) == TRUE){
          if(all(df[,temp] %in% 0:1) == TRUE){
            x = rev(table(df[,temp]))[1] 
            n = sum(table(df[,temp]))
            test = z.test(x = x, n = n, p = input$prop, conf.level = input$conf_level, alternative = input$alternative)
            print("1-sample proportions test")
            print(paste("Null probability ",input$prop))
            print(paste("p-value ",round(test$pval,4)))
            if (input$alternative == "less"){
              print(paste("Alternative hypothesis: true p is less than ",input$prop))
            }
            else if (input$alternative == "greater"){
              print(paste("Alternative hypothesis: true p is greater than ",input$prop))
            }
            else if (input$alternative == "two.sided"){
              print(paste("Alternative hypothesis: true p is not equal to ",input$prop))
            }
            print(paste(100*input$conf_level,"percent confidence interval:"))
            print(paste(round(test$cint,4)))
            print(paste("sample estimates: ",round(test$estimate,4)))
          }
        }
      }
    }
  })

}

shinyApp(ui = ui, server = server)