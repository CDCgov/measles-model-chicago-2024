# Run characterization of counterfactual scenarios

library(tidyverse)

# select value of R0 to use
input_simulations_path <- "output/simulations"
n_trajectories_plot <- 100

scenarios_to_plot <- tibble(
  label = c("No vax, no int", "Baseline"),
  scenario = c("No_vaccination", "March_08"),
  int_day = c("No_intervention", "March_08")
)

# Read in data -----------------------------------------------------------------

config <- seirMeasles::read_params(
  system.file("params.yaml", package = "seirMeasles", mustWork = TRUE)
)

#' Check that the arrow dataset has unique label/iteration/day combinations,
#' which means eg there are not multiple, unexpected gridded parameter values
check_scenarios <- function(df) {
  stopifnot(nrow(collect(head(df))) > 0)

  ns <- df |>
    count(scenario, int_day, sim, day) |>
    select(n) |>
    distinct() |>
    collect() |>
    pull(n)

  stopifnot(all(ns == 1))
  df
}

simulations <- arrow::open_dataset(input_simulations_path) |>
  mutate(across(r_0, as.numeric)) |>
  select(scenario, int_day, r_0, sim, day, reported_cases)

# check that there is only one R0 value
simulations |>
  check_scenarios()

case_series <- read_csv("input/case_series.csv")

# Plot a selection of simulations ----------------------------------------------

simulations |>
  inner_join(scenarios_to_plot) |>
  filter(day < 150, sim <= n_trajectories_plot) |>
  collect() |>
  mutate(date = seirMeasles::day_to_date(
    day,
    start_date = config$history$start_date
  )) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = reported_cases, group = sim), alpha = 0.1) +
  scale_x_date(breaks = "1 month", date_labels = "%b") +
  geom_point(
    data = case_series,
    aes(y = observed_cases),
    color = "orange"
  ) +
  facet_grid(rows = vars(label)) +
  labs(
    title = "Scenario projections",
    x = NULL,
    y = "Cumulative no. reported cases"
  ) +
  cowplot::theme_cowplot()

ggsave("output/diagnostic_scenarios.png", bg = "white")

# Table 2 ----------------------------------------------------------------------

table2_data <- simulations |>
  collect() |>
  group_by(scenario, int_day, sim) |>
  mutate(incident_reported_cases = diff(c(0, reported_cases))) |>
  summarize(
    final_size = max(reported_cases),
    last_day = max(day[incident_reported_cases > 0]),
    .groups = "drop"
  ) |>
  mutate(
    final_size_additional = final_size - 1,
    last_date = seirMeasles::day_to_date(
      last_day,
      start_date = config$history$start_date
    )
  ) |>
  mutate(size_cat_additional = cut(
    final_size_additional,
    c(0, 1, 10, 50, 100, Inf),
    c("0", "1-9", "10-49", "50-99", "100+"),
    right = FALSE
  ))

table2_sizes <- table2_data |>
  count(scenario, int_day, size_cat_additional) |>
  group_by(scenario, int_day) |>
  mutate(p = scales::percent(n / sum(n), 1)) |>
  ungroup() |>
  select(scenario, int_day, size_cat_additional, p) |>
  pivot_wider(
    names_from = size_cat_additional,
    values_from = p,
    values_fill = "0%"
  )

table2_last_dates <- table2_data |>
  group_by(scenario, int_day) |>
  summarize(median_last_date = median(last_date), .groups = "drop")

table2 <- full_join(
  table2_sizes, table2_last_dates,
  by = c("scenario", "int_day")
) |>
  mutate(
    scenario = factor(
      scenario,
      levels = c("No_vaccination", "March_15", "March_08", "March_01")
    ),
    int_day = factor(int_day, levels = c("No_intervention", "March_08"))
  ) |>
  arrange(scenario, int_day) |>
  # drop the not-meaningful combination
  filter(!(scenario == "No_vaccination" & int_day == "March_08"))

write_csv(table2, "output/table2.csv")

# Figure 1 --------------------------------------------------------------------

figure1_data <- table2_data |>
  filter(final_size >= 3) |>
  filter(!(scenario == "No_vaccination" & int_day == "March_08")) |>
  mutate(label = interaction(
    factor(int_day, levels = c("March_08", "No_intervention")),
    factor(scenario, levels = c(
      "March_01", "March_08", "March_15", "No_vaccination"
    )),
    sep = "\n"
  ))

figure1_data |>
  ggplot(aes(label, final_size)) +
  # coef=NULL puts the whisker limits at min/max
  geom_boxplot(coef = NULL) +
  scale_y_continuous(limits = c(0, 300), expand = c(0, 0)) +
  labs(
    title = "Distributions of outbreak sizes, counterfactual scenarios",
    x = "Active case finding / Vaccination",
    y = "No. measles cases"
  ) +
  cowplot::theme_cowplot()

ggsave("output/figure1.png", bg = "white")

figure1_summary <- figure1_data |>
  group_by(scenario, int_day) |>
  summarize(
    min = min(final_size),
    q25 = quantile(final_size, 0.25),
    median = median(final_size),
    q75 = quantile(final_size, 0.75),
    max = max(final_size),
    .groups = "drop"
  )

write_csv(figure1_summary, "output/figure1_summary.csv")
