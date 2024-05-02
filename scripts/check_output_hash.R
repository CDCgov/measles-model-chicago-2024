# Run all the seirMeasles scripts, and check that the output hasn't changed

output_root <- "output"

# clean out everything in output
unlink(file.path(output_root, "*"), expand = TRUE, recursive = TRUE)

# run all the scripts
source("scripts/run_abc.R")
source("scripts/run_simulations.R")
source("scripts/run_scenarios.R")
source("scripts/run_forecasts.R")

# hash all the files in output

current <- tibble(path = list.files(
  output_root,
  recursive = TRUE,
  full.names = TRUE
)) |>
  mutate(hash = map_chr(path, rlang::hash_file))

# keep track of a gold standard of those file contents
gold_path <- file.path("scripts/gold.csv")

if (file.exists(gold_path)) {
  gold <- read_csv(gold_path, show_col_types = FALSE)

  dplyr::full_join(
    gold,
    current,
    by = "hash",
    suffix = c(".gold", ".current")
  ) |>
    print()
} else {
  write_csv(current, gold_path)
  print("Saved new gold data")
}
