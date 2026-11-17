source("R/backtest.R")

cfg <- yaml::read_yaml("config/mrp.yml")
# cfg$backtests$variants <- cfg$backtests$variants["production"]

cfg$run$inference <- "fast"
cfg$run$national_calibration$draws_path <- "data-raw/validation-data/2019-10N/national-vote-draws.csv"
cfg$run$national_calibration$enabled <- T
cfg$run$national_calibration$strength <- 0.5

tmp_config <- tempfile(fileext = ".yml")
yaml::write_yaml(cfg, tmp_config)

run_backtest(
    election = "2019-10N",
    config_path = tmp_config,
    rebuild = FALSE
)
