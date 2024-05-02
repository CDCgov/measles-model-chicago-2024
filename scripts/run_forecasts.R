# Produce forecasts of outbreak size and timing

library(tidyverse)

# Parameters ----------------------------------------------------------

# number of trajectories to keep
n_trajectories <- 100
input_path <- "output/simulations"

scenario_of_interest <- tibble(
  scenario = "March_08",
  int_day = "March_08"
)

future::plan(future::multicore)

# Data -----------------------------------------------------------------

case_series <- read_csv("input/case_series.csv")

config <- seirMeasles::read_params(
  system.file("params.yaml", package = "seirMeasles", mustWork = TRUE)
)

#' Check that there is only one scenario in the data frame
check_one_scenario <- function(df) {
  stopifnot(nrow(df) > 0)
  stopifnot(all(count(df, sim, day)$n == 1))
  df
}

# Read in simulation output, keeping only one scenario
simulations <- arrow::open_dataset(input_path) |>
  # note: Arrow erroneously reads R0 partitions as integer, not float
  mutate(across(r_0, as.numeric)) |>
  inner_join(scenario_of_interest) |>
  select(sim, day, reported_cases) |>
  collect() |>
  check_one_scenario() |>
  # add incident cases
  arrange(sim, day) |>
  group_by(sim) |>
  mutate(incident_reported_cases = diff(c(0, reported_cases))) |>
  ungroup() |>
  # merge in case series
  mutate(date = seirMeasles::day_to_date(
    day,
    start_date = config$history$start_date
  )) |>
  left_join(case_series, by = c("day", "date"))

# Filter trajectories -----------------------------------------------

#' Score a single trajectory
score_trajectory <- function(df) {
  sum(abs(df$observed_cases - df$reported_cases), na.rm = TRUE)
}

#' Score a single trajectory, filtering by date
score_trajectory_as_of <- function(df, as_of_date) {
  stopifnot("date" %in% names(df))
  df |>
    filter(date <= as_of_date) |>
    score_trajectory()
}

#' Select indices of best-scoring trajectories
#'
#' @return vector of `sim`s
filter_trajectories <- function(df, as_of, n = n_trajectories) {
  out <- df |>
    nest(data = -sim) |>
    mutate(
      score = map_dbl(
        data,
        function(x) score_trajectory_as_of(x, as_of)
      ),
      # "row_number" assigns ranks to each value
      rank = row_number(score)
    ) |>
    filter(rank <= n) |>
    pull(sim)

  stopifnot(length(out) == n)

  out
}

forecast_dates <- ymd(c(
  "2024-03-11", "2024-03-18", "2024-03-25", "2024-04-01",
  "2024-04-08"
))

selected_trajectories <- tibble(forecast_date = forecast_dates) |>
  mutate(sim = map(
    forecast_dates,
    \(x) filter_trajectories(simulations, x)
  )) |>
  unnest_longer(sim) |>
  left_join(simulations, by = "sim", relationship = "many-to-many")

# show the trajectories by forecast date
selected_trajectories |>
  filter(between(day, 20, 75)) |>
  ggplot(aes(date)) +
  facet_grid(rows = vars(forecast_date)) +
  geom_line(aes(y = reported_cases, group = sim), alpha = 0.25) +
  geom_point(
    # mask observed data from before forecast date
    data = filter(
      selected_trajectories,
      date <= forecast_date,
      !is.na(observed_cases)
    ),
    aes(y = observed_cases), color = "red"
  ) +
  geom_vline(aes(xintercept = forecast_date), linetype = 2) +
  cowplot::theme_cowplot() +
  labs(
    x = NULL,
    y = "Cumulative no. reported cases",
    title = "Calibrated forecasts, by forecast date"
  )

ggsave("output/diagnostic_forecasts.png", bg = "white")

# Table 1 -------------------------------------------------------------

iqr <- function(x) {
  q50 <- round(median(x))
  q25 <- round(quantile(x, 0.25))
  q75 <- round(quantile(x, 0.75))
  str_glue("{q50} ({q25} to {q75})")
}

#' Show percent different like +25%
#'
#' @param x value
#' @param xf reference value
#'
#' @return string like `"+25%"`
format_pct_difference <- function(x, xf = max(case_series$observed_cases)) {
  prefix <- case_match(
    sign(x - xf),
    1 ~ "+",
    -1 ~ "-",
    0 ~ ""
  )

  value <- abs(x - xf) / xf

  paste0(prefix, scales::percent(value))
}

table1_observed <- case_series |>
  filter(date %in% forecast_dates) |>
  select(date, observed_cases)

table1_forecasts <- selected_trajectories |>
  group_by(forecast_date, sim) |>
  summarize(
    final_size = max(reported_cases),
    last_case_day = max(date[incident_reported_cases > 0]),
    .groups = "drop"
  ) |>
  group_by(forecast_date) |>
  summarize(
    final_size_iqr = iqr(final_size),
    pct_difference = format_pct_difference(median(final_size)),
    median_last_case_day = median(last_case_day),
    .groups = "drop"
  )

table1 <- full_join(
  table1_observed,
  table1_forecasts,
  by = c("date" = "forecast_date")
)

write_csv(table1, "output/table1.csv")
