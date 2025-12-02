# shinyApp(ui = ui, server = server)
tab_menu_sql <- list()

tab_menu_sql$uiOutput <- uiOutput("tab_menu_sql.body")

tab_menu_sql$server <- function(input, output, session, auth) {
  # Sidebar menu for report tab
  output$tab_menu_sql.sidebarpanel <- renderUI({
    sidebarMenu(
      menuItem("SQL", tabName = "sql_tab", icon = icon("database"))
    )
  })
  
  
  # Initial table rendering
  output$tab_menu_sql.body <- renderUI({
    tabItem(
      tabName = "admin_tab",
      fluidRow(
        # Các bộ lọc nằm trong một box filter với khung
        div(
          style = "width: 100%; background-color: #f7f7f7; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
          
          # Row chứa các bộ lọc (STORE, NCC, MCH4, TRẠNG THÁI)
          fluidRow(
            aceEditor("sql_query",
                      mode = "sql",
                      theme = "chrome",
                      height = "300px",
                      value = "SELECT * FROM file_order WHERE `Mã CH` = '2011' LIMIT 2",
                      wordWrap = TRUE),  # Bật tính năng tự động xuống dòng
          ),
          
          # Thêm nút Search để lọc
          fluidRow(
            column(12, 
                   actionButton("run_sql", "Chạy SQL")
            ))
        )
      ),
      DT::dataTableOutput("result_table_sql")
    )
  })
  observeEvent(input$run_sql, {
    query <- input$sql_query  # Lấy câu lệnh SQL từ aceEditor
    
    # Thực thi câu lệnh SQL
    if (nzchar(query)) {
      tryCatch({
        result <- dbGetQuery(conn, query)
        
        # Hiển thị kết quả lên bảng
        output$result_table_sql <- DT::renderDataTable({
          result
        })
      }, error = function(e) {
        # print(e$message)
        showNotification(paste("Warning: " , e$message), type = "error")
      })
    }
  })
  
}

