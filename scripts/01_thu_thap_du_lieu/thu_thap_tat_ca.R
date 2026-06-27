#!/usr/bin/env Rscript

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

required_packages <- c("dplyr", "readr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(readr)
library(tibble)

count_rows <- function(path) {
  if (!file.exists(path)) return(0L)
  nrow(read_csv(path, show_col_types = FALSE))
}

run_source_step <- function(label, script_path, function_name, output_path, stop_on_error = FALSE) {
  started <- Sys.time()
  result <- tryCatch({
    source(script_path, local = TRUE)
    fn <- get(function_name)
    fn()
    tibble(
      source = label,
      status = "ok",
      rows = count_rows(output_path),
      output = output_path,
      seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1),
      message = ""
    )
  }, error = function(e) {
    if (stop_on_error) stop(e)
    tibble(
      source = label,
      status = "skipped_or_failed",
      rows = count_rows(output_path),
      output = output_path,
      seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1),
      message = conditionMessage(e)
    )
  })
  print(result)
  result
}

run_scrape_all <- function(stop_on_error = identical(Sys.getenv("SCRAPE_STOP_ON_ERROR", "0"), "1")) {
  steps <- list(
    list("chotot", PATHS$chotot_scraper_script, "run_scrape", PATHS$chotot_raw_csv),
    list("alonhadat", PATHS$alonhadat_scraper_script, "run_alonhadat_scrape", PATHS$alonhadat_raw_csv),
    list("luachonnhadat", PATHS$luachon_scraper_script, "run_luachon_scrape", PATHS$luachon_raw_csv),
    list("muaban", PATHS$muaban_scraper_script, "run_muaban_scrape", PATHS$muaban_raw_csv),
    list("mogi_crawl", PATHS$mogi_scraper_script, "run_mogi_scrape", PATHS$mogi_scraped_csv),
    list("alonhadat_local", PATHS$import_alonhadat_local_script, "run_import_alonhadat_local", PATHS$alonhadat_local_raw_csv),
    list("mogi_import", PATHS$import_mogi_script, "run_import_mogi", PATHS$mogi_raw_csv),
    list("homedy_import", PATHS$import_homedy_script, "run_import_homedy", PATHS$homedy_raw_csv)
  )

  summary <- bind_rows(lapply(steps, function(step) {
    run_source_step(
      label = step[[1]],
      script_path = step[[2]],
      function_name = step[[3]],
      output_path = step[[4]],
      stop_on_error = stop_on_error
    )
  }))

  cat("\nTong ket thu thap/import:\n")
  print(summary)
  invisible(summary)
}

if (sys.nframe() == 0) {
  run_scrape_all()
}
