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
    # input$butt_select
    if (is.null(input$data_file)) {
      return(NULL)
    } else {
      if (input$type_jobs == "CI") {
        isolate({
          updateTabsetPanel(session, "out1", selected = "Confidence Interval")
        })
      }
      if (input$type_jobs == "one") {
        isolate({
          updateTabsetPanel(session, "out1", selected = "One-sample testing")
        })
      }
      if (input$type_jobs == "two") {
        isolate({
          updateTabsetPanel(session, "out1", selected = "Two-sample testing")
        })
      }
    }
  })
  observe({
    if (is.null(input$butt_test_2)) {
      return(NULL)
    }
    if (input$butt_test_2 == 0) {
      return(NULL)
    }
    isolate({
      updateTabsetPanel(session, "out1", selected = "Two-sample testing")
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
        quote = input$quote
      ),
      "data_csv" = read.csv((input$data_file)$datapath,
        header = input$header, sep = input$sep,
        quote = input$quote
      )
    )
  })
  ## View data
  output$table_data <- renderDataTable(
    {
      if (is.null(dataInput())) {
        return(NULL)
      } else {
        dataInput()
      }
    },
    options = list(pageLength = 10)
  )
  ## CI tab
  ## Choose the variable
  output$var_ci <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    # input$butt_select
    isolate({
      selectInput("selected_var_CI", "Select variable",
        as.list(names(dataInput())),
        selected = NULL, multiple = FALSE
      )
    })
  })
  ## Choose the confidence level
  output$ci_level <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    input$butt_select
    if (input$type_jobs == "CI") {
      radioButtons("selected_CI", "Select confidence level",
        choices = c("0.90", "0.95", "0.99"), selected = "0.95"
      )
    }
  })
  output$butt_run_ci <- renderUI({
    # if(is.null(input$butt_select)) return(NULL)
    # if(input$butt_select == 0) return(NULL)
    actionButton("butt_ci", "Construct the confidence interval")
  })
  ## CI result
  output$ci <- renderUI({
    # if(is.null(input$butt_select)) return(NULL)
    if (is.null(input$butt_ci)) {
      return(NULL)
    }
    if (input$butt_ci == 0) {
      return(NULL)
    }
    input$butt_ci
    isolate({
      mean_x <- mean(dataInput()[, input$selected_var_CI], na.rm = TRUE)
      sd_x <- sd(dataInput()[, input$selected_var_CI], na.rm = TRUE)
      df_ci <- sum(!is.na(dataInput()[, input$selected_var_CI])) - 1
      ci_x <- mean_x + c(-1, 1) * qt((1 + as.numeric(input$selected_CI)) / 2, df = df_ci) * sd_x / sqrt(df_ci + 1)
      withMathJax(sprintf("A %d%% confidence interval for the population mean is:
                            $$[%.03f, %.03f]$$", as.numeric(input$selected_CI) * 100, ci_x[1], ci_x[2]))
    })
  })
  ## Tab one-sample testing
  ## Choose the variable for test
  output$var_test_1 <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    # input$butt_select
    isolate({
      selectInput("selected_var_test_1", "Select variable",
        as.list(names(dataInput())),
        selected = NULL, multiple = FALSE
      )
    })
  })
  output$mean_0 <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    # input$butt_select
    isolate({
      numericInput("selected_mean_0", "The hypothetical value of population mean", value = 0)
    })
  })
  output$hypothesis <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    if (is.null(input$selected_mean_0)) {
      return(NULL)
    }
    # input$butt_select
    isolate({
      radioButtons(
        inputId = "null_hypo", label = "The null hypothesis",
        choiceNames = list(
          paste("\u03BC =", input$selected_mean_0),
          paste("\u03BC \u2265", input$selected_mean_0),
          paste("\u03BC \u2264", input$selected_mean_0)
        ),
        choiceValues = list("two.sided", "less", "greater")
      )
    })
  })
  ## Choose the significance level
  output$signf_level <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    # input$butt_select
    isolate({
      radioButtons("selected_signf", "Select significance level",
        choices = c("0.10", "0.05", "0.01"), selected = "0.05"
      )
    })
  })
  ## test_result
  output$butt_run_test_1 <- renderUI({
    # if(is.null(input$butt_select)) return(NULL)
    # if(input$butt_select == 0) return(NULL)
    actionButton("butt_test_1", "Do the statictical test")
  })
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
        res_test <<- t.test(
          x = dataInput()[, input$selected_var_test_1], alternative = input$null_hypo,
          mu = input$selected_mean_0, conf.level = 1 - as.numeric(input$selected_signf)
        )
        p_value <- ifelse(res_test$p.value < 0.001, "< 0.001", res_test$p.value)
        res_tab <- c(
          round(res_test$estimate, 4),
          round(sd(dataInput()[, input$selected_var_test_1], na.rm = TRUE), 4),
          round(res_test$statistic, 4), p_value
        )
        tab_1 <- cbind(res_tab)
        rownames(tab_1) <- c("Sample mean", "Sample standard deviation", "t-statistic", "p-value")
        colnames(tab_1) <- c("Value")
      })
      as.data.frame(tab_1)
    },
    digits = 4,
    bordered = FALSE,
    align = "lr",
    striped = TRUE,
    rownames = TRUE
  )
  ##
  output$conclusion_1 <- renderUI({
    if (is.null(input$butt_test_1)) {
      return(NULL)
    }
    if (input$butt_test_1 == 0) {
      return(NULL)
    }
    input$butt_test_1
    isolate({
      if (res_test$p.value < as.numeric(input$selected_signf)) {
        h5(paste("There is enough evidence at the ", as.numeric(input$selected_signf) * 100,
          "% significance level to reject the null hypothesis",
          sep = ""
        ))
      } else {
        h5(paste("There is not enough evidence at the ", as.numeric(input$selected_signf) * 100,
          "% significance level to reject the null hypothesis",
          sep = ""
        ))
      }
    })
  })
  ## Tab Two-sample testing
  output$var_test_2_1 <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    # input$butt_select
    isolate({
      selectInput("selected_var_test_2_1", "Select first group",
        as.list(names(dataInput())),
        selected = NULL, multiple = FALSE
      )
    })
  })
  output$var_test_2_2 <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    # input$butt_select
    isolate({
      drop <- input$selected_var_test_2_1
      names_2 <- names(dataInput())
      selectInput("selected_var_test_2_2", "Select second group",
        as.list(names_2[!(names_2 %in% drop)]),
        selected = NULL, multiple = FALSE
      )
    })
  })
  output$hypothesis_2 <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    # input$butt_select
    isolate({
      radioButtons(
        inputId = "null_hypo_2", label = "The null hypothesis",
        choiceNames = list(
          HTML("<p>\U03BC<sub>1</sub> = \U03BC<sub>2</sub></p>"),
          HTML("<p>\U03BC<sub>1</sub> \u2265 \U03BC<sub>2</sub></p>"),
          HTML("<p>\U03BC<sub>1</sub> \u2264 \U03BC<sub>2</sub></p>")
        ),
        choiceValues = list("two.sided", "less", "greater")
      )
    })
  })
  output$check_paired <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    # input$butt_select
    isolate({
      checkboxInput(inputId = "indepe", label = "Two groups are independent", value = TRUE)
    })
  })
  output$check_variance <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    if (is.null(input$type_jobs)) {
      return(NULL)
    }
    if (isTRUE(input$indepe)) {
      isolate({
        checkboxInput(inputId = "unequal_var", label = "The variances are unequal", value = TRUE)
      })
    }
  })
  ## Choose the significance level
  output$signf_level_2 <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
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
    actionButton("butt_test_2", "Do the statictical test")
  })
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
        res_test_2 <<- t.test(
          x = dataInput()[, input$selected_var_test_2_1],
          y = dataInput()[, input$selected_var_test_2_2],
          alternative = input$null_hypo_2, paired = isFALSE(input$indepe),
          var.equal = isFALSE(input$unequal_var),
          conf.level = 1 - as.numeric(input$selected_signf_2)
        )
        p_value_2 <- ifelse(res_test_2$p.value < 0.001, "< 0.001", res_test_2$p.value)
        if (isTRUE(input$indepe)) {
          res_tab_2 <- c(
            round(res_test_2$estimate[1], 4), round(res_test_2$estimate[2], 4),
            round(sd(dataInput()[, input$selected_var_test_2_1], na.rm = TRUE), 4),
            round(sd(dataInput()[, input$selected_var_test_2_2], na.rm = TRUE), 4),
            round(res_test_2$statistic, 4), p_value_2
          )
          tab_2 <- cbind(res_tab_2)
          rownames(tab_2) <- c(
            "Sample mean of 1st group", "Sample mean of 2nd group",
            "Sample standard deviation of 1st group",
            "Sample standard deviation of 2nd group", "t-statistic", "p-value"
          )
          colnames(tab_2) <- c("Value")
        } else {
          res_tab_2 <- c(
            round(res_test_2$estimate, 4),
            round(sd(dataInput()[, input$selected_var_test_2_1] -
              dataInput()[, input$selected_var_test_2_2], na.rm = TRUE), 4),
            round(res_test_2$statistic, 4), p_value_2
          )
          tab_2 <- cbind(res_tab_2)
          rownames(tab_2) <- c(
            "Sample mean of the differences",
            "Sample standard deviation of the differences", "t-statistic", "p-value"
          )
          colnames(tab_2) <- c("Value")
        }
      })
      as.data.frame(tab_2)
    },
    digits = 4,
    bordered = FALSE,
    align = "lr",
    striped = TRUE,
    rownames = TRUE
  )
  ##
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
