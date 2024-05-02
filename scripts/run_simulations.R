# Run simulations, for forecasts and scenarios

library(tidyverse)

seed <- 13
# number of simulations
n_reps <- 10000
out_path <- "output/simulations"

future::plan(future::multicore)

# Read in parameters ----------------------------------------------------------
config <- seirMeasles::read_params(
  system.file("params.yaml", package = "seirMeasles", mustWork = TRUE)
)

# setup parameter variations
parameter_grid <- expand.grid(c(
  list(sim = 1:n_reps),
  config$params
))

base_import_days <- config$history$import_days

base_vaccine_doses <- config$history$vaccine_doses
base_vaccine_days <- config$history$vaccine_days +
  config$history$immunity_onset_delay

# Scenario running functions --------------------------------------------------

report <- function(msg) {
  cat(paste(msg, date(), "", sep = "\n"))
}

#' Write day as date
#'
#' @param dt date
#'
#' @return string in form `"March_08"`
format_day <- function(x) {
  format(
    seirMeasles::day_to_date(x, start_date = config$history$start_date),
    "%B_%d"
  )
}

#' Run and write a scenario to disk
#'
#' @param scenario name
#' @param ... Additional parameters passed to
#'   [seirMeasles::generate_inputs()]
write_scenario <- function(scenario, ...) {
  report(scenario)

  seirMeasles::simulate_grid(
    parameter_grid,
    manual_inputs = seirMeasles::generate_inputs(
      import_days = base_import_days,
      ...
    ),
    seed = seed
  ) |>
    mutate(
      scenario = !!scenario,
      int_day = case_when(
        is.na(int_day) ~ "No_intervention",
        TRUE ~ format_day(int_day)
      )
    ) |>
    arrange(scenario, int_day, r_0) |>
    group_by(scenario, int_day, r_0) |>
    arrow::write_dataset(out_path)
}

# Run scenarios ---------------------------------------------------------------

# clear the output location
unlink(out_path, recursive = TRUE)

write_scenario(
  "No_vaccination"
)

write_scenario(
  "March_08",
  vaccine_doses = base_vaccine_doses,
  vaccine_days = base_vaccine_days
)

write_scenario(
  "March_01",
  vaccine_doses = base_vaccine_doses,
  vaccine_days = base_vaccine_days - 7
)

write_scenario(
  "March_15",
  vaccine_doses = base_vaccine_doses,
  vaccine_days = base_vaccine_days + 7
)

report("Complete!")
