if (.Platform$OS.type == "windows" &&
    grepl("^C($|\\.)", Sys.getlocale("LC_CTYPE"))) {
  suppressWarnings(try(
    Sys.setlocale("LC_CTYPE", "Spanish_Spain.utf8"),
    silent = TRUE
  ))
}

source("R/config.R", encoding = "UTF-8")
source("R/data.R", encoding = "UTF-8")
source("R/availability.R", encoding = "UTF-8")
source("R/calibration.R", encoding = "UTF-8")
source("R/models.R", encoding = "UTF-8")
source("R/reporting.R", encoding = "UTF-8")
source("R/participation.R", encoding = "UTF-8")
source("R/vote-reports.R", encoding = "UTF-8")
source("R/pipeline.R", encoding = "UTF-8")

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  unknown <- setdiff(args, "--rebuild")
  if (length(unknown)) {
    stop(
      "Usage: Rscript R/run.R [--rebuild]. Unknown arguments: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  run_mrp(rebuild = "--rebuild" %in% args)
}
