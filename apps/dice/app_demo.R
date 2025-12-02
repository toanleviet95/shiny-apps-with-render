rm(list = ls())
gc()

packages <- c(
  "shiny", "shinyjs", "glue", "sodium", "shinydashboard", 
  "DT", "googlesheets4", "googledrive", "RSQLite", "pool", "plotly",
  "dplyr", "shinyalert", "pivottabler", "shinyAce"
)
invisible(lapply(packages, library, character.only = TRUE))

# load global parameters (DB connections, login credentials, etc)
source('global.R')
# load ui/server from each tab
source('./modules/00_tab_login.R') 
source('./modules/01_tab_admin.R')
source('./modules/02_tab_problem_coin_toss.R')
source('./modules/02_tab_problem_dice_roll.R')
source('./modules/10_tab_sql.R')

# app.title <- 'An Unified Shiny Portal'

ui <- dashboardPage(
  dashboardHeader(title = "App demo", uiOutput("logoutbtn")), 
  dashboardSidebar(uiOutput("dynamic_sidebar")),
  dashboardBody(
    shinyjs::useShinyjs(),
    includeCSS("www/styles.css"),  # Nạp tệp CSS vào ứng dụng Shiny
    tab_login$uiOutput,
    tabItems(
      tabItem(tabName = "admin_tab", tab_menu_admin$uiOutput),
      # tabItem(tabName = "coin_toss", tab_coin_toss$uiOutput),
      tabItem(tabName = "dice_roll", tab_dice_roll$uiOutput),
      tabItem(tabName = "sql_tab", tab_menu_sql$uiOutput)
    )
  ),
  skin = "red"
)


server <- function(input, output, session) {
  # Reactive values to store login status
  auth <- reactiveValues(logged_in = FALSE, user_id = NULL)
  # Load login server
  tab_login$server(input, output, session, auth)
  # print(auth$logged_in)
  # Render sidebar động dựa trên quyền
  output$dynamic_sidebar <- renderUI({
    req(auth$logged_in)
    
    menu_items <- list(
      menuItem("Problem", tabName = "use_case_tab", icon = icon("chart-bar"),
               menuSubItem("Tung đồng xu", tabName = "coin_toss", icon = icon("circle")),
               menuSubItem("Tung 2 xúc xắc", tabName = "dice_roll", icon = icon("dice-five"))),
      menuItem("SQL", tabName = "sql_tab", icon = icon("database"))
    )
    
    # Thêm Admin menu nếu là admin
    if (auth$user_id == "admin") {
      menu_items <- c(list(menuItem("Admin", tabName = "admin_tab", icon = icon("user-cog"))), menu_items)
    }
    
    sidebarMenu(id = "menu_tabs", .list = menu_items)
  })
  
  # Tải server logic theo quyền
  observe({
    req(auth$logged_in)
    
    # Load modules cơ bản (cho tất cả users)
    # tab_coin_toss$server(input, output, session)
    tab_dice_roll$server(input, output, session)
    
    # Load modules nâng cao (chỉ cho admin)
    if (auth$user_id == "admin") {
      tab_menu_admin$server(input, output, session)
      tab_menu_sql$server(input, output, session, auth)
    }
    
    # Set default tab
    updateTabItems(session, "menu_tabs", selected = "use_case_tab")
  })
}

# Run the application
# shinyApp(ui = ui, server = server)
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

