library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Hypothesis Testing for Means"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      fileInput(inputId = "file", label = "Choose your CSV file"),
      tags$hr(), #Adds a horizontal line 
      checkboxInput(inputId = "header", label = "Header", value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel(title = "Data", dataTableOutput(outputId = "contents")),
        tabPanel(title = "One Sample Testing", 
                 uiOutput(outputId = "vars"),
                 uiOutput(outputId = "m0"),
                 uiOutput(outputId = "signif_one"),
                 textOutput(outputId = "alt_one"),
                 verbatimTextOutput(outputId = "one_sample_result")
        ),
        tabPanel(title = "Two Sample Testing",
                 uiOutput(outputId = "vars_1"),
                 uiOutput(outputId = "vars_2"),
                 uiOutput(outputId = "type"),
                 uiOutput(outputId = "variance"),
                 uiOutput(outputId = "signif_two"),
                 textOutput(outputId = "alt_two"),
                 verbatimTextOutput(outputId = "two_sample_result"))
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
  
  #ONE SAMPLE
  output$vars <- renderUI({
    df <- data()
    selectInput(inputId = "selected_var", label = "Select variable", choices = colnames(df[,c(-1)]))
  })
  
  output$m0 <- renderUI({
    numericInput(inputId = "mean", label = "Choose mean value", value = 0)
  })
  
  output$signif_one <- renderUI({
    selectInput(inputId = "signif_level_1", label = "Significant Level", choices = c(0.01, 0.05, 0.1))
  })
  
  output$alt_one <- renderText({
    print("Type of alternative hypothesis: Two tailed test")
  })
  
  output$one_sample_result <- renderPrint({
    temp = input$selected_var
    df = data()
    sig_1 = as.numeric(input$signif_level_1)
    conf_1 = 1 - sig_1
    if(is.numeric(df[,temp]) == TRUE)
    {
      if(all(df[,temp] %in% 0:1) == FALSE) {t.test(df[,temp], mu = input$mean, alternative = "two.sided", conf.level = conf_1) }
      else {print("Please choose a different variable")} #Binary 
    }
    #Categorical
    else
      print("Please choose a different variable")
  })
  
  #TWO SAMPLE
  output$vars_1 <- renderUI({
    df <- data()
    selectInput(inputId = "selected_var_1", label = "Select variable 1", choices = colnames(df[,c(-1)]))
  })
  
  output$vars_2 <- renderUI({
    df <- data()
    selectInput(inputId = "selected_var_2", label = "Select variable 2", choices = colnames(df[,c(-1)]))
  })
  
  output$type <- renderUI({
    radioButtons(inputId = "type_of_sample", label = "Type of sample", choices = c("Paired", "Independent"), selected = "Independent")
  })
  
  output$variance <- renderUI({
    if (is.null(input$type_of_sample) == FALSE){
      if (input$type_of_sample == "Independent"){
        radioButtons(inputId = "var", label = "Type of test", choices = c("Equal variance", "Unequal variance"), selected = "Unequal variance")
      }
    }
  })
  
  output$signif_two <- renderUI({
    selectInput(inputId = "signif_level_2", label = "Significant Level", choices = c(0.01, 0.05, 0.1))
  })
  
  
  output$alt_two <- renderText({
    print("Type of alternative hypothesis: Two tailed test")
  })
  
  output$two_sample_result <- renderPrint({
    temp1 = input$selected_var_1
    temp2 = input$selected_var_2
    df = data()
    sig_2 = as.numeric(input$signif_level_2)
    conf_2 = 1 - sig_2
    
    if (is.null(input$type_of_sample) == FALSE){
      if (temp1 != temp2){
        if (input$type_of_sample == "Paired"){
          if(is.numeric(df[,temp1]) == TRUE && is.numeric(df[,temp2]) == TRUE){
            if(all(df[,temp1] %in% 0:1) == FALSE && all(df[,temp2] %in% 0:1) == FALSE) {
              t.test(df[,temp1], df[,temp2], alternative = "two.sided", conf.level = conf_2, paired = TRUE)
            }
            else {print("Please check your variable")} #Binary 
          }
          #Categorical
          else
            print("Please check your variable")
        }
        
        else if (input$type_of_sample == "Independent"){
          if(is.numeric(df[,temp1]) == TRUE && is.numeric(df[,temp2]) == TRUE){
            #no binary
            if(all(df[,temp1] %in% 0:1) == FALSE && all(df[,temp2] %in% 0:1) == FALSE){
              if (input$var == "Equal variance")
                t.test(df[,temp1], df[,temp2], alternative = "two.sided", conf.level = conf_2, var.equal = TRUE)
              else if (input$var == "Unequal variance")
                t.test(df[,temp1], df[,temp2], alternative = "two.sided", conf.level = conf_2, var.equal = FALSE)
            }
            #one variable is binary
            else if(all(df[,temp1] %in% 0:1) == TRUE || all(df[,temp2] %in% 0:1) == TRUE){
              #temp1 is binary
              if (all(df[,temp1] %in% 0:1) == TRUE){
                if (input$var == "Equal variance")
                  t.test(df[,temp2] ~ df[,temp1], alternative = "two.sided", conf.level = conf_2, var.equal = TRUE)
                else if (input$var == "Unequal variance")
                  t.test(df[,temp2] ~ df[,temp1], alternative = "two.sided", conf.level = conf_2, var.equal = FALSE)
              }
              #temp2 is binary
              else if (all(df[,temp2] %in% 0:1) == TRUE){
                if (input$var == "Equal variance")
                  t.test(df[,temp1] ~ df[,temp2], alternative = "two.sided", conf.level = conf_2, var.equal = TRUE)
                else if (input$var == "Unequal variance")
                  t.test(df[,temp1] ~ df[,temp2], alternative = "two.sided", conf.level = conf_2, var.equal = FALSE)
              }
            }
          }
          else
            print("Please check your variable")
        } 
      }
      else print("Please check your variable") 
    }
  })
}

shinyApp(ui = ui, server = server)