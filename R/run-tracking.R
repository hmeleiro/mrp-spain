library(dplyr)
source("R/run.R")

tracking_path <- normalizePath(
  "data-raw/tracking-29N/tracking-29N.RDS",
  winslash = "/",
  mustWork = TRUE
)

config <- mrp_load_config("config/mrp_tracking_29N.yml")

config$resolved_paths$raw_vote <- tracking_path
config$resolved_paths$raw_turnout <- tracking_path

result <- run_mrp(.config = config)
