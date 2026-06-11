source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

DIA_CHI_UNG_DUNG <- "127.0.0.1"
CONG_UNG_DUNG <- 3838L

lay_pid_theo_cong <- function(cong) {
  if (Sys.which("lsof") == "") return(character())
  pids <- tryCatch(
    system2("lsof", c("-ti", paste0("tcp:", cong)), stdout = TRUE, stderr = FALSE),
    warning = function(w) character(),
    error = function(e) character()
  )
  unique(pids[nzchar(pids)])
}

cong_dang_ban <- function(cong) {
  length(lay_pid_theo_cong(cong)) > 0
}

giai_phong_cong <- function(cong, thoi_gian_cho_giay = 8) {
  pids <- lay_pid_theo_cong(cong)
  if (length(pids) == 0) return(invisible(TRUE))

  message("Port ", cong, " dang ban boi PID: ", paste(pids, collapse = ", "), ". Dang dung process cu...")
  system2("kill", c("-TERM", pids), stdout = FALSE, stderr = FALSE)

  deadline <- Sys.time() + thoi_gian_cho_giay
  while (Sys.time() < deadline) {
    Sys.sleep(0.25)
    if (!cong_dang_ban(cong)) return(invisible(TRUE))
  }

  remaining <- lay_pid_theo_cong(cong)
  if (length(remaining) > 0) {
    message("Port ", cong, " van ban. Force kill PID: ", paste(remaining, collapse = ", "))
    system2("kill", c("-KILL", remaining), stdout = FALSE, stderr = FALSE)
    Sys.sleep(0.5)
  }

  if (cong_dang_ban(cong)) {
    stop("Khong the giai phong port ", cong, ". Hay kiem tra process dang giu port nay.")
  }

  invisible(TRUE)
}

giai_phong_cong(CONG_UNG_DUNG)

shiny::runApp(".", host = DIA_CHI_UNG_DUNG, port = CONG_UNG_DUNG, launch.browser = FALSE)
