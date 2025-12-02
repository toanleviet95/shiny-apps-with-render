library(shiny)
library(tools)

shinyServer(function(input, output, session){
  ## Update Tabs
  observe({
    # if (is.null(input$butt_select)) return(NULL)
    # if (input$butt_select == 0) return(NULL)
    if(input$type_jobs == "CI"){
      isolate({
        # input$butt_select
        updateTabsetPanel(session, "out1", selected = "Confidence Interval")
      })
      } else{
        isolate({
          # input$butt_select
          updateTabsetPanel(session, "out1", selected = "Hypothesis testing")
        })
      }
    
  })
  observe({
    if(is.null(input$butt_test)) return(NULL)
    if(input$butt_test == 0) return(NULL)
    input$butt_test
    isolate({ 
      updateTabsetPanel(session, "out1", selected = "Hypothesis testing")
    })
  })
  ## CI tab
  ## Choose the confidence level
  output$ci_level <- renderUI({
    if(is.null(input$type_jobs)) return(NULL)
    # input$butt_select
    if(input$type_jobs == "CI"){
      radioButtons("selected_CI", "Select confidence level", 
                   choices = c("0.90", "0.95", "0.99"), selected = "0.95")
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
    if(is.null(input$butt_ci)) return(NULL)
    if(input$butt_ci == 0) return(NULL)
    input$butt_ci
    isolate({
      mean_x <- input$p_hat
      n <- input$num_trails
      sd_x <- sqrt(mean_x*(1 - mean_x)/n)
      ci_x <- mean_x + c(-1,1)*qnorm((1 + as.numeric(input$selected_CI))/2)*sd_x
      withMathJax(sprintf("A %d%% confidence interval for the population mean is:
                            $$[%.03f, %.03f]$$", as.numeric(input$selected_CI)*100, ci_x[1], ci_x[2]))
    })
  })
  ## Tab hypothesis testing
  output$p_0 <- renderUI({
    if(is.null(input$type_jobs)) return(NULL)
    # input$butt_select
    isolate({
      numericInput("selected_p_0", "The hypothetical value of population proportion", value = 0.1, min = 0, max = 1)
    })
  })
  output$hypothesis <- renderUI({
    if(is.null(input$type_jobs)) return(NULL)
    if(is.null(input$selected_p_0)) return(NULL)
    isolate({
      radioButtons(inputId = "null_hypo", label = "The null hypothesis",
                   choiceNames = list(paste("p =", input$selected_p_0), 
                                      paste("p \u2265", input$selected_p_0), 
                                      paste("p \u2264", input$selected_p_0)),
                   choiceValues = list("two.sided", "less", "greater"))
    })
  })
  ## Choose the significance level
  output$signf_level <- renderUI({
    if(is.null(input$type_jobs)) return(NULL)
    # input$butt_select
    isolate({
      radioButtons("selected_signf", "Select significance level", 
                   choices = c("0.10", "0.05", "0.01"), selected = "0.05")
    })
  })
  ## test_result
  output$butt_run_test <- renderUI({
    # if(is.null(input$butt_select)) return(NULL)
    # if(input$butt_select == 0) return(NULL)
    actionButton("butt_test", "Do the statictical test")
  })
  output$res_test <- renderTable({
    # if(is.null(input$butt_select)) return(NULL)
    if(is.null(input$butt_test)) return(NULL)
    if(input$butt_test == 0) return(NULL)
    input$butt_test
    isolate({
      z.test <- function(n, p0, phat, signif.level, alt){
        if (length(p0) > 0){
          se <- sqrt(p0*(1 - p0)/n)
          z <- (phat - p0)/se
          pval <- switch(alt, 
                         two.sided = 2*(1 - pnorm(abs(z))),
                         less = pnorm(z),
                         greater = 1 - pnorm(z)
                         )
        }
        return(list(estimate = phat, statistics = z, p.value = pval))
      }
      res_test <<- z.test(n = input$num_trails, p0 = input$selected_p_0, phat = input$p_hat,
                         signif.level = as.numeric(input$selected_signf), alt = input$null_hypo)
      p_value <- ifelse(res_test$p.value < 0.001, "< 0.001", res_test$p.value)
      res_tab <- c(round(res_test$estimate, 4), round(input$num_trails*input$selected_p_0, 4),
                   round(input$num_trails*(1 - input$selected_p_0), 4),
                   round(res_test$statistic, 4), p_value)
      tab_1 <- data.frame(Value = res_tab)
      rownames(tab_1) <- c("Sample proportion", "\\(np_0\\)", "\\(n(1 - p_0)\\)",  "z-statistic", "p-value")
    })
    tab_1
  }, digits = 4, bordered = FALSE, align = "r", striped = TRUE, include.rownames = TRUE, include.colnames = TRUE)
  output$tableUI <- renderUI({
    # if(is.null(input$butt_select)) return(NULL)
    if(is.null(input$butt_test)) return(NULL)
    if(input$butt_test == 0) return(NULL)
    input$butt_test
    tagList(
      withMathJax(),
      withMathJax(tableOutput("res_test"))
    )
  })
  ##
  output$conclusion <- renderUI({
    if(is.null(input$butt_test)) return(NULL)
    if(input$butt_test == 0) return(NULL)
    input$butt_test
    isolate({
      if(res_test$p.value < as.numeric(input$selected_signf)){
        h5(paste("There is enough evidence at the ", as.numeric(input$selected_signf)*100,
                 "% significance level to reject the null hypothesis", sep = ""))
      } else{
        h5(paste("There is not enough evidence at the ", as.numeric(input$selected_signf)*100,
                 "% significance level to reject the null hypothesis", sep = ""))
      }
    })
  })
})
