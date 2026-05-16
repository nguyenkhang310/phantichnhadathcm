if (dir.exists("R_libs")) {
  .libPaths(c(normalizePath("R_libs"), .libPaths()))
}

APP_HOST <- "127.0.0.1"
APP_PORT <- 3838L

free_port <- function(port) {
  if (Sys.which("lsof") == "") return(invisible(FALSE))

  pid_output <- tryCatch(
    system2("lsof", c("-ti", paste0("tcp:", port)), stdout = TRUE, stderr = FALSE),
    warning = function(w) character(),
    error = function(e) character()
  )

  pids <- unique(pid_output[nzchar(pid_output)])
  if (length(pids) == 0) return(invisible(FALSE))

  message("Port ", port, " dang ban. Dang dung tien trinh: ", paste(pids, collapse = ", "))
  for (pid in pids) {
    try(system2("kill", c("-TERM", pid), stdout = FALSE, stderr = FALSE), silent = TRUE)
  }
  Sys.sleep(0.8)

  still_busy <- tryCatch(
    system2("lsof", c("-ti", paste0("tcp:", port)), stdout = TRUE, stderr = FALSE),
    warning = function(w) character(),
    error = function(e) character()
  )

  still_busy <- unique(still_busy[nzchar(still_busy)])
  if (length(still_busy) > 0) {
    message("Port ", port, " van ban. Ep dung tien trinh: ", paste(still_busy, collapse = ", "))
    for (pid in still_busy) {
      try(system2("kill", c("-KILL", pid), stdout = FALSE, stderr = FALSE), silent = TRUE)
    }
    Sys.sleep(0.4)
  }

  invisible(TRUE)
}

free_port(APP_PORT)
shiny::runApp(".", host = APP_HOST, port = APP_PORT, launch.browser = FALSE)
