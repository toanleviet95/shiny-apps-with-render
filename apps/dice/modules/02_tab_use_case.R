# Load global variables
tab_use_case <- list()

# Define UI for the use case tab
tab_use_case$uiOutput <- uiOutput("tab_use_case.body")


tab_use_case$server <- function(input, output, session, service_account_path) {
  
  # Main content of the use case tab
  output$tab_use_case.body <- renderUI({
    tabItem(
      tabName = "use_case_tab",
      fluidRow(
        box(
          title = "User Data",
          status = "info",
          solidHeader = TRUE, 
          hr(),
          DT::dataTableOutput("admin_user_results")
        )
      )
    )
  })
  
  
  output$admin_user_results <- DT::renderDataTable({
    datatable(dbGetQuery(conn, "SELECT 
                                      user_id,
                                      name,
                                      passod,
                                      permission,
                                      strftime('%Y-%m-%d %H:%M:%S', start_edit_time, 'unixepoch', '+7 hours') start_edit_time,
                                      strftime('%Y-%m-%d %H:%M:%S', end_edit_time, 'unixepoch', '+7 hours') end_edit_time
                                FROM file_user"), 
              options = list(searching = FALSE, 
                             paging = TRUE,
                             autoWidth = TRUE),  
              rownames = FALSE,
              class = "display compact"
    )
  })  
}
