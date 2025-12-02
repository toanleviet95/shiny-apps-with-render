# Load global variables
tab_menu_admin <- list()


# Define UI for the admin tab
tab_menu_admin$uiOutput <- tabItem(
  tabName = "admin_tab",
  fluidRow(
    box(
      downloadButton("download_db", "Lưu trữ database", class = "btn-primary btn-lm")
    )
  ),
  fluidRow(
    box(
      title = "User Data",
      status = "info",
      solidHeader = TRUE, 
      actionButton("get_data_user", "Lấy dữ liệu user từ GS", icon = icon("cloud-upload"), 
                   class = "btn-success btn-lm"),
      hr(),
      DT::dataTableOutput("admin_user_results")
    )
  )
)


tab_menu_admin$server <- function(input, output, session, service_account_path) {
  # Sidebar menu for admin tab
  output$tab_menu_admin.sidebarpanel <- renderUI({
    sidebarMenu(
      menuItem("Admin", tabName = "admin_tab", icon = icon("user-cog"))
    )
  })
  
  # Function to get data from Google Sheets (Users)
  observeEvent(input$get_data_user, {
    shinyjs::runjs("
      $('#get_data_user').html('Đang tải dữ liệu user...');
      $('#get_data_user').css({'background-color': 'red', 'color': 'white','font-family': 'Arial, Helvetica, sans-serif','font-size': '16px'});
    ")
    
    # Authenticate and load user data
    service_account_path <- file.path(getwd(), "data/scct-banhmi-f58a7b51fd59.json")
    gs4_auth(path = service_account_path)
    file_user <- read_sheet("https://docs.google.com/spreadsheets/d/1ksB8068HAqLuiAFH2fTfwFxpdE-kJLmgLVYACku9M8I/edit?gid=516743586#gid=516743586", range = "file_user")
    file_user <- as.data.frame(file_user)
    # Convert list columns if necessary
    list_cols <- sapply(file_user, function(x) is.list(x))
    file_user[list_cols] <- lapply(file_user[list_cols], as.character)
    file_user$start_edit_time <- as.POSIXct(file_user$start_edit_time, format = "%Y-%m-%d %H:%M:%S")
    file_user$end_edit_time <- as.POSIXct(file_user$end_edit_time, format = "%Y-%m-%d %H:%M:%S")
    dbExecute(conn, "DELETE FROM file_user_temp")
    DBI::dbWriteTable(conn, "file_user_temp", file_user, append = TRUE)
    
    # Thêm dữ liệu
    sql_query_insert <- paste0("INSERT OR IGNORE INTO file_user (user_id, passod, name, start_edit_time, end_edit_time, permission)
                                  SELECT user_id, passod, name, start_edit_time, end_edit_time, permission
                                  FROM file_user_temp;
                                  ")
    dbExecute(conn, sql_query_insert)
    # Cập nhật dữ liệu
    sql_query_update <- paste0("UPDATE file_user
                                SET name = (SELECT name FROM file_user_temp WHERE file_user_temp.user_id = file_user.user_id),
                                    permission = (SELECT permission FROM file_user_temp WHERE file_user_temp.user_id = file_user.user_id),
                                    start_edit_time = (SELECT start_edit_time FROM file_user_temp WHERE file_user_temp.user_id = file_user.user_id),
                                    end_edit_time = (SELECT end_edit_time FROM file_user_temp WHERE file_user_temp.user_id = file_user.user_id)
                                WHERE EXISTS (
                                    SELECT 1 FROM file_user_temp WHERE file_user.user_id = file_user_temp.user_id AND file_user_temp.is_dup = 1)")
    
    # Thực thi câu lệnh SQL để cập nhật dữ liệu
    dbExecute(conn, sql_query_update)
    
    shinyjs::runjs("
      $('#get_data_user').html('Lấy dữ liệu user từ GS');
      $('#get_data_user').css({'background-color': '', 'color': ''});
    ")
    shinyalert("Success", "Tải dữ liệu user thành công", type = "success")
    
    # Reload the user data in the table
    output$admin_user_results <- DT::renderDataTable({
      file_user_data <- dbGetQuery(conn, "SELECT
                                      user_id,
                                      name,
                                      passod,
                                      permission,
                                      strftime('%Y-%m-%d %H:%M:%S', start_edit_time, 'unixepoch', '+7 hours') start_edit_time,
                                      strftime('%Y-%m-%d %H:%M:%S', end_edit_time, 'unixepoch', '+7 hours') end_edit_time
                                FROM file_user")
      datatable(file_user_data,
                options = list(searching = FALSE,
                               paging = TRUE,
                               autoWidth = TRUE),
                rownames = FALSE,
                class = "display compact"
      )
    })
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
  
  # Download database
  output$download_db <- downloadHandler(
    filename = function() {
      "wcm_ssct.db"  # Tên file khi tải xuống
    },
    content = function(file) {
      file.copy("wcm_ssct", file)  # Đường dẫn đến file SQLite gốc
    }
  )  
}
