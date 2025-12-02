library(RSQLite)

# Kết nối đến database SQLite
conn <- dbPool(RSQLite::SQLite(), dbname = "data/database_user")
# Tạo bảng nếu chưa tồn tại
dbExecute(conn, "CREATE TABLE IF NOT EXISTS file_order (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                column1 TEXT,
                column2 TEXT,
                column3 REAL)")

#Đường dẫn truy vấn dữ liệu google sheet
service_account_path <- './Data/scct-banhmi-f58a7b51fd59.json'