# Load global variables
tab_dice_roll <- list()


tab_dice_roll$uiOutput <-
  tabItem(tabName = "dice_tab",
          fluidRow(
            box(
              title = "Mô phỏng tung xúc xắc", width = 3, status = "primary", solidHeader = TRUE,
              
              actionButton("dice_roll_once", "TUNG XÚC XẮC", 
                           style = "color: white; background-color: #28a745; font-weight: bold; padding: 8px; width: 100%;"),
              
              br(),
              fluidRow(
                column(
                  width = 12,  # Thay vì width = 3, offset = 2
                  style = "text-align: center;",  # Căn giữa
                  div(
                    style = "display: flex; justify-content: center; gap: 10px;",  # 🔹 căn giữa + khoảng cách
                    imageOutput("dice1", height = "100px"),
                    imageOutput("dice2", height = "100px")
                  )
                )
              ),
              
              tags$hr(),
              
              p("Bạn có thể nhập số lần để tung nhiều lần"),
              numericInput("dice_num_rolls", "Số lần tung:", 
                           value = 1, min = 1, max = 1000, step = 1,
                           width = "100%"),
              actionButton("dice_roll_multiple", "TUNG NHIỀU LẦN", 
                           style = "color: white; background-color: #007bff; font-weight: bold; padding: 8px; width: 100%;"),
              br(), br(),
              
              div(
                style = "text-align: center;",
                actionButton("reset", "RESET", 
                             style = "color: white; background-color: #dc3545; font-weight: bold; padding: 8px; width: 50%;")
              )
            ),
            box(
              title = "Kết quả", width = 9, status = "info", solidHeader = TRUE,
              fluidRow(
                infoBoxOutput("dice_current_sum_value", width = 3),
                infoBoxOutput("dice_total_rolls", width = 3)
                # infoBoxOutput("dice_min_sum", width = 2),
                # infoBoxOutput("dice_max_sum", width = 2),
                # infoBoxOutput("dice_mode_sum", width = 2)
              ),
              div(
                style = "display: flex; width: 100%; margin: 0; padding: 0;",
                div(
                  style = "width: 41.66%; margin: 0; padding: 0; margin-right: -1px;",
                  box(
                    title = "Bảng thống kê tần suất tổng điểm", 
                    status = "primary", solidHeader = TRUE, width = 12,
                    style = "padding-top: 0; padding-bottom: 0;",
                    DTOutput("dice_frequency_table")
                  )
                ),
                div(
                  style = "width: 58.34%; margin: 0; padding: 0; margin-left: -1px;",
                  box(
                    title = "Biểu đồ xác suất tổng số chấm", 
                    status = "info", solidHeader = TRUE, width = 12,
                    style = "margin: 0; border-radius: 0; border-left: none; height: 100%;",
                    plotlyOutput("dice_probability_plot", height = "350px")
                  )
                )
              ), 
              br(),
              # Lịch sử kết quả
              fluidRow(
                column(6,
                       box(
                         title = "Xem kết quả tung xúc xắc 10 lần gần nhất", 
                         status = "info", solidHeader = TRUE, width = 12,
                         tableOutput("dice_recent_history_table"),
                       ),
                       downloadButton("dice_download_data", "Tải xuống toàn bộ dữ liệu",
                                      style = "color: white; background-color: #17a2b8; font-weight: bold;")
                )
              )
              
            )
          )
         
  )


# Server
tab_dice_roll$server <- function(input, output, session) {
  
  # Tạo reactive dice_values để lưu trữ kết quả
  dice_values <- reactiveValues(
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
    dice_values$roll_count <- dice_values$roll_count + 1
    new_row <- data.frame(
      Lần = dice_values$roll_count,
      Xúc_xắc_1 = dice1,
      Xúc_xắc_2 = dice2,
      Tổng = dice1 + dice2
    )
    dice_values$history <- rbind(dice_values$history, new_row)
  }
  
  # Xử lý sự kiện khi nhấn nút tung 1 lần
  observeEvent(input$dice_roll_once, {
    dice_values$dice1 <- roll_dice()
    dice_values$dice2 <- roll_dice()
    add_to_history(dice_values$dice1, dice_values$dice2)
  })
  
  # Xử lý sự kiện khi nhấn nút tung nhiều lần
  observeEvent(input$dice_roll_multiple, {
    dice_num_rolls <- input$dice_num_rolls
    
    if (dice_num_rolls > 0) {
      # Hiển thị thông báo
      showModal(modalDialog(
        title = "Đang tung xúc xắc...",
        paste("Đang thực hiện", dice_num_rolls, "lần tung"),
        footer = NULL
      ))
      
      # Thực hiện nhiều lần tung
      for (i in 1:dice_num_rolls) {
        dice1 <- roll_dice()
        dice2 <- roll_dice()
        add_to_history(dice1, dice2)
      }
      
      # Cập nhật kết quả hiện tại với lần tung cuối cùng
      dice_values$dice1 <- dice1
      dice_values$dice2 <- dice2
      
      # Đóng thông báo
      removeModal()
      
      # Hiển thị thông báo hoàn thành
      showNotification(
        paste("Đã hoàn thành", dice_num_rolls, "lần tung!"),
        type = "message",
        duration = 3
      )
    }
  })
  
  # Xử lý sự kiện reset
  observeEvent(input$reset, {
    dice_values$dice1 <- 1
    dice_values$dice2 <- 1
    dice_values$history <- data.frame(
      Lần = integer(),
      Xúc_xắc_1 = integer(),
      Xúc_xắc_2 = integer(),
      Tổng = integer(),
      stringsAsFactors = FALSE
    )
    dice_values$roll_count <- 0
    
    showNotification("Đã reset tất cả dữ liệu!", type = "warning", duration = 3)
  })
  
  # Hiển thị hình ảnh xúc xắc 1
  output$dice1 <- renderImage({
    outfile <- tempfile(fileext = '.png')
    
    png(outfile, width = 100, height = 100)
    par(mar = c(0,0,0,0))
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
         xlab = "", ylab = "", axes = FALSE)
    
    # Vẽ hình vuông đại diện cho xúc xắc
    rect(0.1, 0.1, 0.9, 0.9, col = "white", border = "black", lwd = 1)
    
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
    
    for(dot in dots[[as.character(dice_values$dice1)]]) {
      points(dot[1], dot[2], pch = 19, cex = 2, col = "black")
    }
    
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 100,
         height = 100,
         alt = paste("Xúc xắc 1:", dice_values$dice1))
  }, deleteFile = TRUE)
  
  # Hiển thị hình ảnh xúc xắc 2
  output$dice2 <- renderImage({
    outfile <- tempfile(fileext = '.png')
    
    png(outfile, width = 100, height = 100)
    par(mar = c(0,0,0,0))
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
         xlab = "", ylab = "", axes = FALSE)
    
    rect(0.1, 0.1, 0.9, 0.9, col = "white", border = "black", lwd = 1)
    
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
    
    for(dot in dots[[as.character(dice_values$dice2)]]) {
      points(dot[1], dot[2], pch = 19, cex = 2, col = "black")
    }
    
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 100,
         height = 100,
         alt = paste("Xúc xắc 2:", dice_values$dice2))
  }, deleteFile = TRUE)
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  
  # Value Box: Tổng số chấm hiện tại
  output$dice_current_sum_value <- renderInfoBox({
    valueBox(as.character(dice_values$dice1 + dice_values$dice2),"Tổng số chấm", icon = icon("dice"), color = "purple")
  })
  
  # Value Box: Số lần đã tung
  output$dice_total_rolls <- renderValueBox({
    valueBox(as.character(nrow(dice_values$history)), "Số lần đã tung", icon = icon("redo"), color = "green")
  })
  
  # Value Box: Tổng nhỏ nhất
  output$dice_min_sum <- renderValueBox({
    valueBox(if(nrow(dice_values$history) > 0) {
      as.character(min(dice_values$history$Tổng))
    } else {
      "0"
    }
    , "Tổng điểm nhỏ nhất", icon = icon("arrow-down"), color = "yellow")
  })
  
  # Value Box: Tổng lớn nhất
  output$dice_max_sum <- renderValueBox({
    valueBox(if(nrow(dice_values$history) > 0) {
      as.character(max(dice_values$history$Tổng))
    } else {
      "0"
    }, "Tổng điểm lớn nhất", icon = icon("arrow-up"), color = "orange")
  })
  
  # Value Box: Tổng xuất hiện nhiều nhất
  output$dice_mode_sum <- renderValueBox({
    valueBox(if(nrow(dice_values$history) > 0) {
      freq_table <- table(dice_values$history$Tổng)
      mode_value <- as.numeric(names(freq_table)[which.max(freq_table)])
      as.character(mode_value)
    } else {
      "0"
    }, "Xuất hiện nhiều nhất", icon = icon("star"), color = "purple")
  })
  
  # Vẽ biểu đồ xác suất
  output$dice_probability_plot <- renderPlotly({
    theoretical_probs <- data.frame(
      Total = 2:12,
      Theoretical = c(1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36, 3/36, 2/36, 1/36)
    )
    
    if (nrow(dice_values$history) > 0) {
      actual_probs <- dice_values$history %>%
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
          xaxis = list(
            title = "Tổng số chấm",
            tickmode = "array",
            tickvals = 2:12,  # Hiển thị tất cả giá trị từ 2-12
            ticktext = 2:12
          ),
          yaxis = list(title = "Xác suất"),
          legend = list(
            orientation = "h",
            x = 0.5,          # Căn giữa theo chiều ngang
            xanchor = "center", # Neo vào trung tâm
            y = -0.3,         # Đẩy legend xuống dưới biểu đồ
            yanchor = "top"   # Neo vào phía trên
          ),
          margin = list(b = 90)  # Tăng margin dưới để chứa legend
        )
      
    } else {
      plot_ly(theoretical_probs, x = ~Total, y = ~Theoretical, type = "bar",
              marker = list(color = "#1D2951"), name = "Lý thuyết") %>%
        layout(
          xaxis = list(
            title = "Tổng số chấm",
            tickmode = "array",
            tickvals = 2:12,
            ticktext = 2:12
          ),
          yaxis = list(title = "Xác suất"),
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.3,
            yanchor = "top"
          ),
          margin = list(b = 90)
        )
    }
  })
  
  # Hiển thị bảng tần suất tổng điểm
  output$dice_frequency_table <- renderDT({
    all_sums <- data.frame(
      Tổng_điểm = 2:12,
      Tần_số = 0,
      Tần_suất = 0
    )
    
    total_row <- data.frame(
      Tổng_điểm = "TỔNG CỘNG",
      Tần_số = 0,
      Tần_suất = 1
    )
    final_table <- rbind(all_sums, total_row)
    
    if (nrow(dice_values$history) > 0) {
      freq_table <- dice_values$history %>%
        group_by(Tổng_điểm = Tổng) %>%
        summarise(
          Tần_số = n(),
          Tần_suất = round(n() / nrow(dice_values$history), 2)
        )  %>%
        mutate(Tổng_điểm = as.character(Tổng_điểm))
      
      total_row <- data.frame(
        Tổng_điểm = "TỔNG CỘNG",
        Tần_số = nrow(dice_values$history),
        Tần_suất = 1,
        stringsAsFactors = FALSE
      )
      
      
      combine_table <- rbind(freq_table, total_row)
      
      # Merge tổng_điểm với freq_table
      final_table <- final_table %>%
        left_join(combine_table, by = "Tổng_điểm", suffix = c(".final", ".freq")) %>%
        mutate(
          Tần_số = coalesce(Tần_số.freq, Tần_số.final, 0),
          Tần_suất   = coalesce(Tần_suất.freq, Tần_suất.final, 0)
        ) %>%
        select(-Tần_số.final, -Tần_số.freq, -Tần_suất.final, -Tần_suất.freq) %>%
        mutate(
          Tổng_điểm = factor(Tổng_điểm, 
                             levels = c(as.character(2:12), "TỔNG CỘNG"))
        ) %>%
        arrange(Tổng_điểm)
      
    }
    
    datatable(
      final_table,
      rownames = FALSE,
      colnames = c("Tổng điểm", "Tần số", "Tần suất"),
      options = list(dom = "t", 
                     paging = FALSE,
                     ordering = FALSE,
                     columnDefs = list(
                       list(className = "dt-center", targets = "_all"),
                       list(width = "30%", targets = 0),   # Tổng điểm
                       list(width = "30%", targets = 1),   # Tần số  
                       list(width = "40%", targets = 2)    # Tần suất (lớn nhất)
                     )),
      escape = FALSE
    ) %>%
      # Hàng TỔNG CỘNG màu xám
      formatStyle(
        "Tổng_điểm",
        backgroundColor = styleEqual("TỔNG CỘNG", "lightgray"),
        fontWeight = styleEqual("TỔNG CỘNG", "bold")
      ) %>%
      formatStyle(
        "Tần_suất",
        background = styleColorBar(
          range(final_table$Tần_suất[final_table$Tổng_điểm != "TỔNG CỘNG"]),
          "lightgreen"
        ),
        backgroundSize = "100% 100%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      # Thêm CSS để giảm chiều cao dòng
      formatStyle(
        columns = names(final_table),
        `font-size` = "14px",
        padding = "3px 4px"  # Giảm padding
      )
    
    
  })
  
  
  # Hiển thị lịch sử gần nhất (10 lần)
  output$dice_recent_history_table <- renderTable({
    if(nrow(dice_values$history) > 0) {
      tail(dice_values$history, 10)
    }
  }, bordered = TRUE, width = "100%")
  
  # Tải xuống dữ liệu
  output$dice_download_data <- downloadHandler(
    filename = function() {
      paste("Tung xúc xắc", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dice_values$history, file, row.names = FALSE)
    }
  )
}
