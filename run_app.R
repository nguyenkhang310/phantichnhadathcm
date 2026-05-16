if (dir.exists("R_libs")) {
  .libPaths(c(normalizePath("R_libs"), .libPaths()))
}

shiny::runApp(".", host = "127.0.0.1", port = 3838, launch.browser = FALSE)
