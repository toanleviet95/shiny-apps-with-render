server <- function(input, output, session) {
  ## Update Tabs
  observe({
    if (is.null(input$file1)) {
      return(NULL)
    }
    isolate({
      updateTabsetPanel(session, "out1", selected = "Data")
    })
  })
  observe({
    # if (is.null(input$butt_select)) return(NULL)
    # if (input$butt_select == 0) return(NULL)
    if (is.null(input$data_file)) {
      return(NULL)
    }
    if (input$type_jobs == "GF") {
      isolate({
        # input$butt_select
        updateTabsetPanel(session, "out1", selected = "Goodness-of-fit Test")
      })
    }
    if (input$type_jobs == "Ind") {
      isolate({
        # input$butt_select
        updateTabsetPanel(session, "out1", selected = "Chi-square Test for Independence")
      })
    }
  })
  observe({
    if (is.null(input$butt_test_1)) {
      return(NULL)
    }
    if (input$butt_test_1 == 0) {
      return(NULL)
    }
    input$butt_test_1
    isolate({
      updateTabsetPanel(session, "out1", selected = "Goodness-of-fit Test")
    })
  })
  ## Define input data set
  obs <- reactiveValues(dat = "none")
  observe({
    if (!is.null(input$data_file)) {
      ext <- file_ext((input$data_file)$name)
      if (ext == "dat" | ext == "txt") obs$dat <- "data_table"
      if (ext == "csv") obs$dat <- "data_csv"
    }
  })
  dataInput <- reactive({
    switch(obs$dat,
      "none" = NULL,
      "data_table" = read.table((input$data_file)$datapath,
        header = input$header, sep = input$sep,
        quote = input$quote, row.names = 1
      ),
      "data_csv" = read.csv((input$data_file)$datapath,
        header = input$header, sep = input$sep,
        quote = input$quote, row.names = 1
      )
    )
  })
  ## View data
  output$table_data <- DT::renderDataTable({
    if (is.null(dataInput())) {
      return(NULL)
    } else {
      datatable(dataInput(), rownames = TRUE)
    }
  })
  ## Goodness-of-fit tab
  ## Input the hypothesis proportion
  output$helptext_1 <- renderUI({
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    isolate({
      h5("Input the expected proportions of categories. The default is unform proportion")
    })
  })
  output$p_0 <- renderUI({
    z <- nrow(dataInput())
    lapply(seq(z), function(i) {
      numericInput(
        inputId = paste0("range", paste0(i)), label = paste0(i, "-th expected proportion"), value = 1 / z,
        min = 0, max = 1
      )
    })
  })
  ## Choose the significant level
  output$signf_level_1 <- renderUI({
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    # input$butt_select
    if (input$type_jobs == "GF") {
      radioButtons("selected_signf_1", "Select significance level",
        choices = c("0.10", "0.05", "0.01"), selected = "0.05"
      )
    }
  })
  # capturing the value of input widgets in a matrix
  cm <- reactive({
    r <- nrow(dataInput())
    changed_m <- numeric(r)
    lapply(seq(r), function(i) {
      x <- input[[paste0("range", paste0(i))]]
      changed_m[i] <<- ifelse(!is.null(x), x, 0)
    })
    changed_m
  })
  output$hypothesis_1_1 <- renderUI({
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    isolate({
      h5("The null hypothesis: the propotions are fit with the expected proportions")
    })
  })
  output$hypothesis_1_2 <- renderUI({
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    isolate({
      h5("The alternative hypothesis: the propotions are not fit with the expected proportions")
    })
  })
  output$butt_run_test_1 <- renderUI({
    # if(is.null(input$butt_select)) return(NULL)
    # if(input$butt_select == 0) return(NULL)
    actionButton("butt_test_1", "Do the goodness-of-fit test")
  })
  ## GF result
  output$show_res_exp_1 <- renderUI({
    if (is.null(input$butt_test_1)) {
      return(NULL)
    }
    if (input$butt_test_1 == 0) {
      return(NULL)
    }
    input$butt_test_1
    isolate({
      h4("The table for the observed and expected frequencies.")
    })
  })
  ##
  output$table_expection_1 <- renderTable(
    {
      if (is.null(input$butt_test_1)) {
        return(NULL)
      }
      if (input$butt_test_1 == 0) {
        return(NULL)
      }
      input$butt_test_1
      isolate({
        obsv <- dataInput()[, 1]
        obsv_expect <- sum(obsv) * cm()
        xx <- rbind(obsv, obsv_expect)
        tab_expect_1 <- data.frame(Value = xx)
        rownames(tab_expect_1) <- c("Observed", "Expected")
        colnames(tab_expect_1) <- rownames(dataInput())
      })
      tab_expect_1
    },
    digits = 4,
    bordered = FALSE,
    align = "r",
    striped = TRUE,
    width = "100%",
    include.rownames = TRUE,
    include.colnames = TRUE
  )
  ##
  output$show_res_test_1 <- renderUI({
    if (is.null(input$butt_test_1)) {
      return(NULL)
    }
    if (input$butt_test_1 == 0) {
      return(NULL)
    }
    input$butt_test_1
    isolate({
      h4("The Chi-squared Goodness-of-fit test.")
    })
  })
  ##
  output$res_test_1 <- renderTable(
    {
      # if(is.null(input$butt_select)) return(NULL)
      if (is.null(input$butt_test_1)) {
        return(NULL)
      }
      if (input$butt_test_1 == 0) {
        return(NULL)
      }
      input$butt_test_1
      isolate({
        p_expect <- cm()
        res_test_1 <<- chisq.test(x = as.numeric(dataInput()[, 1]), p = p_expect)
        p_value <- ifelse(res_test_1$p.value < 0.001, "< 0.001", res_test_1$p.value)
        res_tab <- c(round(res_test_1$statistic, 4), p_value)
        tab_1 <- data.frame(Value = res_tab)
        rownames(tab_1) <- c("Chi-squared statistic", "p-value")
      })
      tab_1
    },
    digits = 4,
    bordered = FALSE,
    align = "r",
    striped = TRUE,
    include.rownames = TRUE,
    include.colnames = TRUE
  )
  output$conclusion_1 <- renderUI({
    if (is.null(input$butt_test_1)) {
      return(NULL)
    }
    if (input$butt_test_1 == 0) {
      return(NULL)
    }
    input$butt_test_1
    isolate({
      if (res_test_1$p.value < as.numeric(input$selected_signf_1)) {
        h5(paste("There is enough evidence at the ", as.numeric(input$selected_signf_1) * 100,
          "% significance level to reject the null hypothesis",
          sep = ""
        ))
      } else {
        h5(paste("There is not enough evidence at the ", as.numeric(input$selected_signf_1) * 100,
          "% significance level to reject the null hypothesis",
          sep = ""
        ))
      }
    })
  })
  ## Tab testing of independence
  output$hypothesis_2_1 <- renderUI({
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    isolate({
      h5("The null hypothesis: Two variables are independent")
    })
  })
  output$hypothesis_2_2 <- renderUI({
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    isolate({
      h5("The alternative hypothesis: Two variables are dependent")
    })
  })
  ## Choose the significance level
  output$signf_level_2 <- renderUI({
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    # input$butt_select
    isolate({
      radioButtons("selected_signf_2", "Select significance level",
        choices = c("0.10", "0.05", "0.01"), selected = "0.05"
      )
    })
  })
  ## test_result
  output$butt_run_test_2 <- renderUI({
    # if(is.null(input$butt_select)) return(NULL)
    # if(input$butt_select == 0) return(NULL)
    actionButton("butt_test_2", "Do the Chi-square test for independence")
  })
  ##
  output$show_plot_mosaic <- renderUI({
    if (is.null(input$butt_test_2)) {
      return(NULL)
    }
    if (input$butt_test_2 == 0) {
      return(NULL)
    }
    input$butt_test_2
    isolate({
      h4("The Mosaic plot for visualizing the dependence of two categorical variables.")
    })
  })
  output$plot_mosaic <- renderPlot({
    if (is.null(input$butt_test_2)) {
      return(NULL)
    }
    if (input$butt_test_2 == 0) {
      return(NULL)
    }
    input$butt_test_2
    isolate({
      mosaicplot(dataInput(), shade = TRUE, main = "Mosaic plot", las = 1, cex.axis = 0.8)
    })
  })
  ##
  output$show_res_test_2 <- renderUI({
    if (is.null(input$butt_test_2)) {
      return(NULL)
    }
    if (input$butt_test_2 == 0) {
      return(NULL)
    }
    input$butt_test_2
    isolate({
      h4("The Chi-squared test for independence.")
    })
  })
  ##
  output$res_test_2 <- renderTable(
    {
      # if(is.null(input$butt_select)) return(NULL)
      if (is.null(input$butt_test_2)) {
        return(NULL)
      }
      if (input$butt_test_2 == 0) {
        return(NULL)
      }
      input$butt_test_2
      isolate({
        res_test_2 <<- chisq.test(x = dataInput())
        p_value <- ifelse(res_test_2$p.value < 0.001, "< 0.001", res_test_2$p.value)
        res_tab <- c(round(res_test_2$statistic, 4), p_value)
        tab_2 <- data.frame(Value = res_tab)
        rownames(tab_2) <- c("Chi-squared statistic", "p-value")
      })
      tab_2
    },
    digits = 4,
    bordered = FALSE,
    align = "r",
    striped = TRUE,
    include.rownames = TRUE,
    include.colnames = TRUE
  )
  output$conclusion_2 <- renderUI({
    if (is.null(input$butt_test_2)) {
      return(NULL)
    }
    if (input$butt_test_2 == 0) {
      return(NULL)
    }
    input$butt_test_2
    isolate({
      if (res_test_2$p.value < as.numeric(input$selected_signf_2)) {
        h5(paste("There is enough evidence at the ", as.numeric(input$selected_signf_2) * 100,
          "% significance level to reject the null hypothesis",
          sep = ""
        ))
      } else {
        h5(paste("There is not enough evidence at the ", as.numeric(input$selected_signf_2) * 100,
          "% significance level to reject the null hypothesis",
          sep = ""
        ))
      }
    })
  })
}
