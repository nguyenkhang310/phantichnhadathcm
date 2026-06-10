source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

APP_HOST <- "127.0.0.1"
APP_PORT <- 3838L

# Hàm port_pids: đếm hoặc kiểm tra điều kiện xử lý.
port_pids <- function(port) {
  if (Sys.which("lsof") == "") return(character())
  pids <- tryCatch(
    system2("lsof", c("-ti", paste0("tcp:", port)), stdout = TRUE, stderr = FALSE),
    warning = function(w) character(),
    error = function(e) character()
  )
  unique(pids[nzchar(pids)])
}

# Hàm port_in_use: đếm hoặc kiểm tra điều kiện xử lý.
port_in_use <- function(port) {
  length(port_pids(port)) > 0
}

# Hàm kill_port: hỗ trợ xử lý dữ liệu trong script.
kill_port <- function(port, timeout_seconds = 8) {
  pids <- port_pids(port)
  if (length(pids) == 0) return(invisible(TRUE))

  message("Port ", port, " dang ban boi PID: ", paste(pids, collapse = ", "), ". Dang dung process cu...")
  system2("kill", c("-TERM", pids), stdout = FALSE, stderr = FALSE)

  deadline <- Sys.time() + timeout_seconds
  while (Sys.time() < deadline) {
    Sys.sleep(0.25)
    if (!port_in_use(port)) return(invisible(TRUE))
  }

  remaining <- port_pids(port)
  if (length(remaining) > 0) {
    message("Port ", port, " van ban. Force kill PID: ", paste(remaining, collapse = ", "))
    system2("kill", c("-KILL", remaining), stdout = FALSE, stderr = FALSE)
    Sys.sleep(0.5)
  }

  if (port_in_use(port)) {
    stop("Khong the giai phong port ", port, ". Hay kiem tra process dang giu port nay.")
  }

  invisible(TRUE)
}

kill_port(APP_PORT)

shiny::runApp(".", host = APP_HOST, port = APP_PORT, launch.browser = FALSE)
