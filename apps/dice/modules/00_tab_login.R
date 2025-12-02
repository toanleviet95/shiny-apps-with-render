# define a list to wrap ui/server for login tab
tab_login <- list() 

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("Userid", placeholder="Userid", label = tagList(icon("user"), "Userid")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#e31837;
                               padding: 10px 15px; width: 150px; cursor: pointer;
                               font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect Userid or password!",
                                  style = "color: red; font-weight: 600; 
                                          padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Userid: admin  Password: 12300"),
                     br(),
                     tags$code("Userid: user  Password: 12300")
                   ))
)

# Giao diện đổi mật khẩu
resetpage <- div(id = "resetpage", 
                 style = "
                   width: 500px; 
                   max-width: 100%; 
                   margin: 0 auto; 
                   padding: 20px; 
                   position: fixed;  /* Đặt vị trí cố định */
                   top: 50%;  /* Căn giữa theo chiều dọc */
                   left: 50%;  /* Căn giữa theo chiều ngang */
                   transform: translate(-50%, -50%);  /* Dịch chuyển để căn chính xác */
                   background-color: #fff;  /* Nền trắng */
                   box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); /* Tạo hiệu ứng bóng đổ */
                   z-index: 9999;  /* Đảm bảo bảng luôn nổi bật trên giao diện */
                 ",
                 wellPanel(
                   tags$h2("Đặt lại mật khẩu", class = "text-center", style = "color:#333; font-weight:600;"),
                   passwordInput("oldPass", placeholder="Mật khẩu cũ", label = tagList(icon("unlock-alt"), "Old Password")),
                   passwordInput("newPassReset", placeholder="Mật khẩu mới", label = tagList(icon("unlock-alt"), "New Password")),
                   actionButton("updatePassword", "Update Password", style = "color: white; background-color:#e31837; width: 150px; font-size: 14px; font-weight: 600;")
                 )
)


# User information panel (left corner)
user_info_panel <- div(id = "user_info_panel", 
                       style = "position: absolute; top: 10px; left: 10px; font-size: 16px; color: #333;",
                       textOutput("user_info"))

header <- dashboardHeader(title = "Đặt hàng bánh mì", uiOutput("logoutbtn"))
tab_login$uiOutput <- uiOutput("tab_login.body")
tab_login$ui <- dashboardPage(
  header,
  dashboardSidebar(uiOutput("tab_login.sidebarpanel")),
  dashboardBody(
    shinyjs::useShinyjs(),
    tab_login$uiOutput,
    user_info_panel # Add user info panel to the body
  ),
  skin = "red"
)

# Server logic
tab_login$server <- function(input, output, session, auth) {
  # Mỗi lần trang được làm mới, thực hiện lại truy vấn cơ sở dữ liệu
  credentials <- reactive({
    # Truy vấn cơ sở dữ liệu mỗi lần trang làm mới
    credentials_data <- dbGetQuery(conn, "
      SELECT
        user_id,
        name AS user_name,
        passod,
        permission,
        strftime('%Y-%m-%d %H:%M:%S', start_edit_time, 'unixepoch', '+7 hours') start_edit_time,
        strftime('%Y-%m-%d %H:%M:%S', end_edit_time, 'unixepoch', '+7 hours') end_edit_time
      FROM file_user
    ")
    
    # Kiểm tra nếu không có dữ liệu hoặc bảng rỗng, thì sử dụng dữ liệu mặc định
    if (is.null(credentials_data) || nrow(credentials_data) == 0) {
      credentials_data <- data.frame(
        user_id = c("admin", "user"),
        passod = sapply(c("12300", "12300"), password_store),
        permission = c("basic", "advanced"),
        stringsAsFactors = FALSE
      )
    } else {
      credentials_data$user_id <- as.character(credentials_data$user_id)
      credentials_data$passod <- as.character(credentials_data$passod)
      credentials_data$passod <- sapply(credentials_data$passod, password_store)
    }
    
    return(credentials_data)
  })
  
  # Hàm lấy thông tin người dùng từ credentials
  get_user_info <- function(Userid) {
    creds <- credentials() %>% filter(user_id == Userid)
    user_info <- list(
      username = creds %>% select(user_name) %>% first() %>% pull(),
      permission = creds %>% select(permission) %>% first() %>% pull(),
      start_edit_time = creds %>% select(start_edit_time) %>% first() %>% pull(),
      end_edit_time = creds %>% select(end_edit_time) %>% first() %>% pull()
    )
    return(user_info)
  }
  
  
  login = FALSE
  USER <- reactiveValues(login = login, Userid = NULL, username = NULL, permission = NULL)
  # auth <- reactiveValues(logged_in = login, userid = NULL, username = NULL, permission = NULL)
  
  # Sau khi người dùng đăng nhập thành công, lưu thông tin vào sessionStorage
  observe({
    if (USER$login == TRUE) {
      shinyjs::runjs('sessionStorage.setItem("loginStatus", "TRUE")')
      shinyjs::runjs(paste0('sessionStorage.setItem("userId", "', as.character(USER$Userid), '")'))
      shinyjs::runjs(paste0('sessionStorage.setItem("permission", "', as.character(USER$permission), '")'))
    }
  })
  
  observe({
    shinyjs::runjs('
    if (sessionStorage.getItem("loginStatus") === "TRUE") {
      var userId = sessionStorage.getItem("userId");
      var permission = sessionStorage.getItem("permission");
      Shiny.setInputValue("userid_from_storage", userId);
      Shiny.setInputValue("permission_from_storage", permission);
    }
  ')
  })
  
  
  observeEvent(input$userid_from_storage, {
    if (!is.null(input$userid_from_storage)) {
      USER$login <- TRUE
      auth$logged_in <- TRUE
      
      USER$Userid <- input$userid_from_storage
      auth$user_id <- input$userid_from_storage
      
      auth$permission <- input$permission_from_storage
      USER$permission <- input$permission_from_storage
    }
  })
  
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Userid <- isolate(input$Userid)
          Password <- isolate(input$passwd)
          # Chuyển đổi thành chuỗi
          Userid <- as.character(Userid)
          Password <- as.character(Password)
          if(length(which(credentials()$user_id==Userid))==1&is.null(input$logoutButton)) { 
            pasmatch  <- credentials()["passod"][which(credentials()$user_id==Userid),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              # Lấy thông tin người dùng
              user_info <- get_user_info(Userid)
              
              # Cập nhật trạng thái người dùng
              USER$login <- auth$logged_in <- TRUE
              USER$Userid <- auth$user_id <- Userid
              USER$username <- auth$username <- user_info$username
              USER$permission <- auth$permission <- user_info$permission
              auth$start_edit_time <- user_info$start_edit_time
              auth$end_edit_time <- user_info$end_edit_time
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    
    # Tạo dropdown menu với tất cả chữ và icon màu trắng
    tags$li(class = "dropdown", style = "padding: 10px; background-color: #d9534f;",  # Đặt màu nền đỏ cho menu
            a(href = "#", 
              class = "dropdown-toggle", 
              `data-toggle` = "dropdown", 
              icon("user", style = "color: white;"),  # Đặt màu icon thành trắng
              span(style = "color: white;", "Hello, ", USER$Userid),  # Đặt màu chữ "Hello, USER$Userid" thành trắng
              span(class = "caret", style = "color: white;")  # Thay đổi màu caret thành trắng
            ),
            tags$ul(class = "dropdown-menu", style = "background-color: #d9534f; color: white; width: 220px;",  # Đặt màu nền đỏ và màu chữ trắng cho menu
                    # tags$li(a(href = "javascript:window.location.reload(true)", icon("sign-out", style = "color: white;"), span(style = "color: white;", "Logout"))),  # Đặt màu chữ Logout thành trắng
                    tags$li(actionButton("logoutButton", "Logout", icon = icon("sign-out"), style = "color: white; background: none; border: none; font-size: 14px; font-weight: normal;")),
                    tags$li(actionButton("resetButton", "Reset Password", icon = icon("key"), style = "color: white; background: none; border: none; font-size: 14px; font-weight: normal; padding: 5px 10px;"))
            )
    )
  })
  
  
  # Lắng nghe sự kiện click vào nút Logout
  observeEvent(input$logoutButton, {
    # Cập nhật trạng thái đăng nhập
    USER$login <- FALSE
    USER$Userid <- NULL
    USER$permission <- NULL
    auth$logged_in <- NULL
    
    # Xóa thông tin trong sessionStorage
    shinyjs::runjs('sessionStorage.clear()')
    
    # Làm mới trang sau khi đăng xuất
    shinyjs::runjs('window.location.reload(true)')
    
    # Reset giá trị của các input
    # updateTextInput(session, "Userid", value = "")  # Reset trường input Userid
    updateTextInput(session, "passwd", value = "")  # Reset trường input passwd
  })
  
  
  # Render user information panel
  output$user_info <- renderText({
    req(USER$login)
    paste("Hello, ", USER$Userid)
  })
  
  output$tab_login.body <- renderUI({
    if (USER$login == FALSE ) {
      loginpage
    }
  })
  
  # Chuyển sang giao diện đổi mật khẩu
  observeEvent(input$resetButton, {
    output$tab_login.body <- renderUI({
      resetpage  # Hiển thị bảng reset khi bấm nút
    })
  })
  
  # Đổi mật khẩu
  observeEvent(input$updatePassword, {
    req(input$oldPass, input$newPassReset)
    
    # Chuyển đổi các giá trị mật khẩu thành chuỗi
    oldPassString <- as.character(input$oldPass)
    newPassResetString <- as.character(input$newPassReset)
    
    # Truy vấn người dùng từ cơ sở dữ liệu
    user <- dbGetQuery(conn, "SELECT * FROM file_user WHERE user_id = ?", list(auth$user_id))
    if (nrow(user) == 1 && !is.na(user$passod)) {
      # Ép kiểu passod thành chuỗi
      passodString <- as.character(user$passod)
      # Kiểm tra mật khẩu cũ với password_verify (passodString đã được mã hóa)
      if (passodString==oldPassString) {
        # Mã hóa mật khẩu mới và lưu vào cơ sở dữ liệu
        dbExecute(conn, "UPDATE file_user SET passod = ? WHERE user_id = ?", 
                  list(newPassResetString, auth$user_id))
        showModal(modalDialog("Password updated successfully!", easyClose = TRUE))
        shinyjs::hide("resetpage")
      } else {
        showModal(modalDialog("Old password is incorrect or username does not exist!", easyClose = TRUE))
      }
    } else {
      showModal(modalDialog("User not found or password field is invalid!", easyClose = TRUE))
    }
  })
  
  # Sử dụng shinyjs để ẩn bảng khi click ra ngoài
  observe({
    # Khi resetpage hiển thị, lắng nghe sự kiện click ra ngoài
    shinyjs::runjs('
    $(document).click(function(event) {
      if (!$(event.target).closest("#resetpage").length) {
        // Nếu click ra ngoài bảng resetpage, ẩn bảng này
        $("#resetpage").hide();
      }
    });
  ')
  })
  
  
}
