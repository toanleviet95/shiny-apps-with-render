library(shiny)
library(tools)

shinyServer(function(input, output, session){
  ## Update Tabs
  observe({
    if(is.null(input$file1)) return(NULL)
    isolate({ 
      updateTabsetPanel(session, "out1", selected = "Data")
    })
  })
  observe({
    if (is.null(input$butt_sum_data)) return(NULL)
    if (input$butt_sum_data == 0) return(NULL)
    isolate({ 
      updateTabsetPanel(session, "out1", selected = "Summary")
    })
  })
  observe({
    if (is.null(input$butt_run_plot)) return(NULL)
    if (input$butt_run_plot == 0) return(NULL)
    isolate({ 
      updateTabsetPanel(session, "out1", selected = "Plot")
    })
  })
  ## Define input data set
  obs <- reactiveValues(dat = "none")
  observe({
    if(!is.null(input$data_file)) {
      ext <- file_ext((input$data_file)$name)
      if(ext == "dat" | ext == "txt") obs$dat <- "data_table"
      if(ext == "csv") obs$dat <- "data_csv"
    } 
  })
  dataInput <- reactive({
    switch(obs$dat,
           "none" = NULL,
           "data_table" = read.table((input$data_file)$datapath, 
                                     header = input$header, sep = input$sep,
                                     quote = input$quote),
           "data_csv" = read.csv((input$data_file)$datapath, 
                                 header = input$header, sep = input$sep,
                                 quote = input$quote)
    )
  })
  ## View data
  output$table_data <- renderDataTable({
    if (is.null(dataInput())) return(NULL)
    else dataInput()
   },
   options = list(pageLength = 10)
  )
  ## Select one variable
  ## Choose the diagnostic test
  output$vars <- renderUI({
    if (is.null(dataInput())) return(NULL)
    selectInput("selected_var", "Select variable", 
                as.list(names(dataInput())), selected = NULL, multiple = FALSE)
  })
  ## Choose the type of variable
  output$type_varible <- renderUI({
    if (is.null(dataInput())) return(NULL)
    if (is.null(input$selected_var)) return(NULL)
    radioButtons("type_vars", "What is type of variable", 
                 choices = c("Categorical variable" = "categ", "Numerical Variable" = "numb"),
                 selected = "numb")
  })
  ## Tab summary
  output$result <- renderText({
    if (is.null(dataInput())) return(NULL)
    input$butt_sum_data
    isolate({
      paste("Summary statistics of ", input$selected_var)
    })
  })
  output$summary <- renderTable({
    if(is.null(dataInput())) return(NULL)
    input$butt_sum_data
    #Numeric
    isolate({
      if(input$type_vars == "numb") {
        u <- c(as.numeric(summary(dataInput()[,input$selected_var])[1:6]), 
               var(dataInput()[,input$selected_var], na.rm = TRUE),
               sd(dataInput()[,input$selected_var], na.rm = TRUE),
               sum(is.na(dataInput()[,input$selected_var])))
        tab <- cbind(u)
        rownames(tab) <- c("Minimum", "Q1", "Median-Q2", "Mean", "Q3", "Maximum", "Variance",
                           "Standard deviation", "Number of missing values")
        colnames(tab) <- c("Value")
      } else {
        res_tab <<- table(dataInput()[,input$selected_var], useNA = "always")
        names(res_tab) <- c(names(res_tab[-length(res_tab)]), "Number of missing values")
        tab <- as.matrix(res_tab)
        colnames(tab) <- c("Frequency")
      }
    })
    as.data.frame(tab)
  }, digits = 3, bordered = FALSE, width = "75%", align = NULL, rownames = TRUE)
  ## Tab plot
  plotInput <- function(){
    if(input$type_vars == "numb"){
      if(input$type_plot == "boxpl") boxplot(dataInput()[,input$selected_var], 
                                             main = paste("Box plot of", input$selected_var),
                                             col = "forestgreen", pch = 16)
      if(input$type_plot == "hist") hist(dataInput()[,input$selected_var], 
                                         main = paste("Histogram of", input$selected_var),
                                         xlab = input$selected_var, col = "grey",
                                         border = FALSE)
      if(input$type_plot == "hist_box"){
        def.par <- par(no.readonly = TRUE)
        layout(mat = matrix(c(1,2), 2, 1, byrow = TRUE), heights = c(2,6))
        par(mar = c(0, 4.5, 1.1, 2.1))
        boxplot(dataInput()[,input$selected_var], horizontal = TRUE, xaxt = "n", 
                pch = 16, col = "forestgreen", frame = FALSE)
        par(mar = c(4.5, 4.5, 1.1, 2.1))
        hist(dataInput()[,input$selected_var], col = "grey", border = FALSE, main = " ", 
             xlab = input$selected_var)
        par(def.par)
      }
    } else {
      if(input$type_plot == "bar_plot") {
        xx <- barplot(res_tab[-length(res_tab)], ylab = "Frequency",
                      main = paste("Bar chart of", input$selected_var))
        text(x = xx, y = as.numeric(res_tab[-length(res_tab)]), 
             label = as.numeric(res_tab[-length(res_tab)]), pos = 1, cex = 1, col = "blue")
      }
    }
  }
  output$plot <- renderPlot({
    if(is.null(dataInput())) return(NULL)
    if(is.null(input$butt_run_plot)) return(NULL)
    if(input$butt_run_plot == 0) return(NULL)
    input$butt_run_plot
    isolate({
      print(plotInput())
    })
  })
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot-descriptive-statistics.png")
    },
    content = function(file) {
      png(file)
      plotInput()
      dev.off()
    }
  )
})
