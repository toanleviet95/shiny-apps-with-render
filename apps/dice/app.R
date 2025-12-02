library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(glue)
library(sodium)
library(shinydashboard)
library(DT)
library(googlesheets4)
library(googledrive)
library(RSQLite)
library(pool)
library(dplyr)
library(shinyalert)

options(shiny.maxRequestSize = 100*1024^2)

source('global.R')
source('./modules/02_tab_problem_coin_toss.R')
source('./modules/02_tab_problem_dice_roll.R')
# source('./modules/01_tab_admin.R')

ui <- dashboardPage(
  dashboardHeader(title = "Demo app"),
  dashboardSidebar(
    sidebarMenu(
      id = "menu_tabs",
      # menuItem("Admin", tabName = "admin_tab", icon = icon("user-cog")),
      menuItem("Problem", tabName = "use_case_tab", icon = icon("chart-bar"),
               # menuSubItem("Tung đồng xu", tabName = "coin_toss", icon = icon("circle")),
               menuSubItem("Tung 2 xúc xắc", tabName = "dice_roll", icon = icon("dice-five"))
      )
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    includeCSS("www/styles.css"),  # Nạp tệp CSS vào ứng dụng Shiny
    tabItems(
      # tabItem(tabName = "admin_tab", tab_menu_admin$uiOutput),
      # tabItem(tabName = "coin_toss", tab_coin_toss$uiOutput),
      tabItem(tabName = "dice_roll", tab_dice_roll$uiOutput)
    )
  ),
  skin = "red"
)


server <- function(input, output, session) {
  # Tải server logic theo quyền
  observe({
      # tab_menu_admin$server(input, output, session)
      # tab_coin_toss$server(input, output, session)
      tab_dice_roll$server(input, output, session)
  })
}

shinyApp(ui = ui, server = server, options = list(port = 3839))
