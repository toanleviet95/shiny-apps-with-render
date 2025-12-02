library(shiny)

ui <- fluidPage(
  # Application title
  titlePanel("Example Module"),

  sidebarLayout(
    position = "left",

    # Sidebar panel
    sidebarPanel(
      style = "overflow-y:scroll; max-height: 850px; position:relative;",
      tags$head(
        tags$style(type = "text/css", "select { max-width: 400px; }"),
        tags$style(type = "text/css", ".span4 { max-width: 400px; }"),
        tags$style(type = "text/css", ".well { max-width: 400px; }")
      ),

      h3("Module Controls"),

      # Add your input controls here
      textInput("text_input", "Enter text:", value = "Hello World"),

      numericInput("numeric_input", "Enter number:", value = 10, min = 1, max = 100),

      actionButton("submit_btn", "Submit", class = "btn-primary")
    ),

    # Main panel
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          title = "Output",
          h4("Results"),
          textOutput("text_output"),
          verbatimTextOutput("numeric_output")
        ),
        tabPanel(
          title = "Plot",
          plotOutput("example_plot")
        )
      )
    )
  )
)