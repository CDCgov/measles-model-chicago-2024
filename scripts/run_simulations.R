# Run simulations, for forecasts and scenarios

library(tidyverse)

output_dir <- "output"

# if multicore parallel processing is available, use it
if (future::supportsMulticore()) {
  future::plan(future::multicore)
}

# Run simulations ----------------------------------------------------------
# clear existing output
parameters_path <- file.path(output_dir, "parameters.rds")
results_path <- file.path(output_dir, "results")

unlink(parameters_path)
unlink(results_path, recursive = TRUE)

# read in experiment specification
spec <- simWrapper::read_spec(
  system.file("params.yaml", package = "seirMeasles", mustWork = TRUE)
)

# run experiment & cache results
simWrapper::cache_experiment(
  fun = seirMeasles::simulate,
  spec = spec,
  parameters_path = file.path(output_dir, "parameters.rds"),
  results_path = file.path(output_dir, "results")
)

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
