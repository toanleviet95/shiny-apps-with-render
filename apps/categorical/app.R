library(shiny)
library(tools)
library(DT)

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
