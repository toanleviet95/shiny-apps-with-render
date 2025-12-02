library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinydashboard)
library(shinyBS)

ui <- dashboardPage(
  dashboardHeader(title = "Mô phỏng Tung 2 Xúc Xắc"),
  
  # Sidebar vẫn còn menu điều hướng, nhưng không chứa nút bấm
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tung xúc xắc", tabName = "dice_tab", icon = icon("dice"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab mô phỏng xúc xắc
      tabItem(tabName = "dice_tab",
              fluidRow(
                box(
                  title = "Mô phỏng tung xúc xắc", width = 3, status = "primary", solidHeader = TRUE,
                  
                  actionButton("roll_once", "TUNG XÚC XẮC", 
                               style = "color: white; background-color: #28a745; font-weight: bold; padding: 8px; width: 100%;"),
                  
                  tags$hr(),
          
                  p("Bạn có thể nhập số lần để tung nhiều lần"),
                  numericInput("num_rolls", "Số lần tung:", 
                               value = 1, min = 1, max = 1000, step = 1,
                               width = "100%"),
                  actionButton("roll_multiple", "TUNG NHIỀU LẦN", 
                               style = "color: white; background-color: #007bff; font-weight: bold; padding: 8px; width: 100%;"),
                  br(),
                  
                  actionButton("reset", "RESET", 
                               style = "color: white; background-color: #dc3545; font-weight: bold; padding: 8px; width: 100%;")
                ),
                box(
                  title = "Kết quả", width = 9, status = "info", solidHeader = TRUE,
                  fluidRow(
                    infoBoxOutput("current_sum_value", width = 2),
                    infoBoxOutput("total_rolls", width = 2),
                    infoBoxOutput("min_sum", width = 2),
                    infoBoxOutput("max_sum", width = 2),
                    infoBoxOutput("mode_sum", width = 2)
                  ),
                  
                  br(),
                  fluidRow(
                    column(
                      width = 8, offset = 2,   # 🔹 căn giữa (chiếm 8/12, lệch 2/12 bên trái)
                      div(
                        style = "display: flex; justify-content: center; gap: 40px;",  # 🔹 căn giữa + khoảng cách
                        imageOutput("dice1", height = "200px"),
                        imageOutput("dice2", height = "200px")
                      )
                    )
                  )
                )
              ),
            fluidRow(
                  column(
                    4,
                    box(
                      title = "Bảng thống kê tần suất tổng điểm", 
                      status = "primary", solidHeader = TRUE, width = 12,
                      DTOutput("frequency_table")
                    )
                  ),
                  column(
                    8,
                    box(
                      title = "Biểu đồ xác suất tổng số chấm", 
                      status = "info", solidHeader = TRUE, width = 12,
                      plotlyOutput("probability_plot", height = "400px")
                    )
                )
            ),
            
            # Lịch sử kết quả
            fluidRow(
              column(6,
                     box(
                       title = "Xem kết quả tung xúc xắc 10 lần gần nhất", 
                       status = "info", solidHeader = TRUE, width = 12,
                       tableOutput("recent_history_table"),
                     ),
                     downloadButton("download_data", "Tải xuống toàn bộ dữ liệu",
                                    style = "color: white; background-color: #17a2b8; font-weight: bold;")
              )
            )
      )
    )
  )
)



# Server
server <- function(input, output, session) {
  
  # Tạo reactive values để lưu trữ kết quả
  values <- reactiveValues(
    dice1 = 1,
    dice2 = 1,
    history = data.frame(
      Lần = integer(),
      Xúc_xắc_1 = integer(),
      Xúc_xắc_2 = integer(),
      Tổng = integer(),
      stringsAsFactors = FALSE
    ),
    roll_count = 0
  )
  
  # Hàm tung xúc xắc
  roll_dice <- function() {
    sample(1:6, 1)
  }
  
  # Hàm thêm kết quả vào lịch sử
  add_to_history <- function(dice1, dice2) {
    values$roll_count <- values$roll_count + 1
    new_row <- data.frame(
      Lần = values$roll_count,
      Xúc_xắc_1 = dice1,
      Xúc_xắc_2 = dice2,
      Tổng = dice1 + dice2
    )
    values$history <- rbind(values$history, new_row)
  }
  
  # Xử lý sự kiện khi nhấn nút tung 1 lần
  observeEvent(input$roll_once, {
    values$dice1 <- roll_dice()
    values$dice2 <- roll_dice()
    add_to_history(values$dice1, values$dice2)
  })
  
  # Xử lý sự kiện khi nhấn nút tung nhiều lần
  observeEvent(input$roll_multiple, {
    num_rolls <- input$num_rolls
    
    if (num_rolls > 0) {
      # Hiển thị thông báo
      showModal(modalDialog(
        title = "Đang tung xúc xắc...",
        paste("Đang thực hiện", num_rolls, "lần tung"),
        footer = NULL
      ))
      
      # Thực hiện nhiều lần tung
      for (i in 1:num_rolls) {
        dice1 <- roll_dice()
        dice2 <- roll_dice()
        add_to_history(dice1, dice2)
      }
      
      # Cập nhật kết quả hiện tại với lần tung cuối cùng
      values$dice1 <- dice1
      values$dice2 <- dice2
      
      # Đóng thông báo
      removeModal()
      
      # Hiển thị thông báo hoàn thành
      showNotification(
        paste("Đã hoàn thành", num_rolls, "lần tung!"),
        type = "message",
        duration = 3
      )
    }
  })
  
  # Xử lý sự kiện reset
  observeEvent(input$reset, {
    values$dice1 <- 1
    values$dice2 <- 1
    values$history <- data.frame(
      Lần = integer(),
      Xúc_xắc_1 = integer(),
      Xúc_xắc_2 = integer(),
      Tổng = integer(),
      stringsAsFactors = FALSE
    )
    values$roll_count <- 0
    
    showNotification("Đã reset tất cả dữ liệu!", type = "warning", duration = 3)
  })
  
  # Hiển thị hình ảnh xúc xắc 1
  output$dice1 <- renderImage({
    outfile <- tempfile(fileext = '.png')
    
    png(outfile, width = 200, height = 200)
    par(mar = c(0,0,0,0))
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
         xlab = "", ylab = "", axes = FALSE)
    
    # Vẽ hình vuông đại diện cho xúc xắc
    rect(0.1, 0.1, 0.9, 0.9, col = "white", border = "black", lwd = 2)
    
    # Vẽ các chấm dựa trên giá trị xúc xắc
    dots <- list(
      `1` = list(c(0.5, 0.5)),
      `2` = list(c(0.25, 0.75), c(0.75, 0.25)),
      `3` = list(c(0.25, 0.75), c(0.5, 0.5), c(0.75, 0.25)),
      `4` = list(c(0.25, 0.75), c(0.75, 0.75), c(0.25, 0.25), c(0.75, 0.25)),
      `5` = list(c(0.25, 0.75), c(0.75, 0.75), c(0.5, 0.5), 
                 c(0.25, 0.25), c(0.75, 0.25)),
      `6` = list(c(0.25, 0.75), c(0.75, 0.75), c(0.25, 0.5), 
                 c(0.75, 0.5), c(0.25, 0.25), c(0.75, 0.25))
    )
    
    for(dot in dots[[as.character(values$dice1)]]) {
      points(dot[1], dot[2], pch = 19, cex = 2, col = "black")
    }
    
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 200,
         height = 200,
         alt = paste("Xúc xắc 1:", values$dice1))
  }, deleteFile = TRUE)
  
  # Hiển thị hình ảnh xúc xắc 2
  output$dice2 <- renderImage({
    outfile <- tempfile(fileext = '.png')
    
    png(outfile, width = 200, height = 200)
    par(mar = c(0,0,0,0))
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
         xlab = "", ylab = "", axes = FALSE)
    
    rect(0.1, 0.1, 0.9, 0.9, col = "white", border = "black", lwd = 2)
    
    dots <- list(
      `1` = list(c(0.5, 0.5)),
      `2` = list(c(0.25, 0.75), c(0.75, 0.25)),
      `3` = list(c(0.25, 0.75), c(0.5, 0.5), c(0.75, 0.25)),
      `4` = list(c(0.25, 0.75), c(0.75, 0.75), c(0.25, 0.25), c(0.75, 0.25)),
      `5` = list(c(0.25, 0.75), c(0.75, 0.75), c(0.5, 0.5), 
                 c(0.25, 0.25), c(0.75, 0.25)),
      `6` = list(c(0.25, 0.75), c(0.75, 0.75), c(0.25, 0.5), 
                 c(0.75, 0.5), c(0.25, 0.25), c(0.75, 0.25))
    )
    
    for(dot in dots[[as.character(values$dice2)]]) {
      points(dot[1], dot[2], pch = 19, cex = 2, col = "black")
    }
    
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 200,
         height = 200,
         alt = paste("Xúc xắc 2:", values$dice2))
  }, deleteFile = TRUE)
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  
  # Value Box: Tổng số chấm hiện tại
  output$current_sum_value <- renderInfoBox({
    valueBox(as.character(values$dice1 + values$dice2),"Tổng số chấm", icon = icon("dice"), color = "purple")
  })
  
  # Value Box: Số lần đã tung
  output$total_rolls <- renderValueBox({
    valueBox(as.character(nrow(values$history)), "Số lần đã tung", icon = icon("redo"), color = "green")
  })
  
  # Value Box: Tổng nhỏ nhất
  output$min_sum <- renderValueBox({
    valueBox(if(nrow(values$history) > 0) {
      as.character(min(values$history$Tổng))
    } else {
      "0"
    }
    , "Tổng điểm nhỏ nhất", icon = icon("arrow-down"), color = "yellow")
  })
  
  # Value Box: Tổng lớn nhất
  output$max_sum <- renderValueBox({
    valueBox(if(nrow(values$history) > 0) {
      as.character(max(values$history$Tổng))
    } else {
      "0"
    }, "Tổng điểm lớn nhất", icon = icon("arrow-up"), color = "orange")
  })
  
  # Value Box: Tổng xuất hiện nhiều nhất
  output$mode_sum <- renderValueBox({
    valueBox(if(nrow(values$history) > 0) {
      freq_table <- table(values$history$Tổng)
      mode_value <- as.numeric(names(freq_table)[which.max(freq_table)])
      as.character(mode_value)
    } else {
      "0"
    }, "Xuất hiện nhiều nhất", icon = icon("star"), color = "purple")
  })
  
  # Vẽ biểu đồ xác suất
  output$probability_plot <- renderPlotly({
    theoretical_probs <- data.frame(
      Total = 2:12,
      Theoretical = c(1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36, 3/36, 2/36, 1/36)
    )
    
    if (nrow(values$history) > 0) {
      actual_probs <- values$history %>%
        group_by(Tổng) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        mutate(Actual = Count / sum(Count))
      
      plot_data <- theoretical_probs %>%
        left_join(actual_probs, by = c("Total" = "Tổng")) %>%
        mutate(Actual = ifelse(is.na(Actual), 0, Actual))
      
      plot_ly(plot_data, x = ~Total) %>%
        add_bars(y = ~Theoretical, name = "Lý thuyết", marker = list(color = "lightgray")) %>%
        add_bars(y = ~Actual, name = "Thực tế", marker = list(color = "#1D2951")) %>%
        layout(
          barmode = "group",
          xaxis = list(title = "Tổng số chấm"),
          yaxis = list(title = "Xác suất"),
          legend = list(orientation = "h", x = 0.3, y = -0.2)
        )
      
    } else {
      plot_ly(theoretical_probs, x = ~Total, y = ~Theoretical, type = "bar",
              marker = list(color = "#1D2951")) %>%
        layout(
          xaxis = list(title = "Tổng số chấm"),
          yaxis = list(title = "Xác suất")
        )
    }
  })
  
  
  # Hiển thị bảng tần suất tổng điểm
  output$frequency_table <- renderDT({
    if (nrow(values$history) > 0) {
      freq_table <- values$history %>%
        group_by(Tổng_điểm = Tổng) %>%
        summarise(
          Tần_suất = n(),
          Tỷ_lệ = round(n() / nrow(values$history), 2)
        ) %>%
        arrange(Tổng_điểm)
      
      total_row <- data.frame(
        Tổng_điểm = "TỔNG CỘNG",
        Tần_suất = nrow(values$history),
        Tỷ_lệ = 1
      )

      final_table <- rbind(freq_table, total_row)
      
      datatable(
        final_table,
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE),
        escape = FALSE
      ) %>%
        # Hàng TỔNG CỘNG màu xám
        formatStyle(
          "Tổng_điểm",
          backgroundColor = styleEqual("TỔNG CỘNG", "lightgray"),
          fontWeight = styleEqual("TỔNG CỘNG", "bold")
        ) %>%
        formatStyle(
          "Tỷ_lệ",
          background = styleColorBar(
            range(final_table$Tỷ_lệ[final_table$Tổng_điểm != "TỔNG CỘNG"]),
            "lightgreen"
          ),
          backgroundSize = "100% 100%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
      
    }
  })
  
  
  
  # Hiển thị lịch sử gần nhất (10 lần)
  output$recent_history_table <- renderTable({
    if(nrow(values$history) > 0) {
      tail(values$history, 10)
    }
  }, bordered = TRUE, width = "100%")
  
  # Tải xuống dữ liệu
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Tung xúc xắc", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$history, file, row.names = FALSE)
    }
  )
}

# Chạy ứng dụng
shinyApp(ui = ui, server = server)