# Run approximate Bayesian computation, for parameter refinement

library(tidyverse)

# Set session values ----------------------------------------------------------
# no. of simulations
n_iter <- 10000
# seed is use for both prior generation AND simulations
seed <- 30
# proportion of trajectories to accept
accept_p <- 0.01

# set up parallel processing
future::plan(future::multicore)

# Load data -------------------------------------------------------------------

case_series <- read_csv("input/case_series.csv")

config <- seirMeasles::read_params(
  system.file("params.yaml", package = "seirMeasles", mustWork = TRUE)
)

fixed_params <- config$params
fixed_params$r_0 <- NULL
fixed_params$int_day <- 40
fixed_params$int_eff <- NULL

fixed_manual_inputs <- seirMeasles::generate_inputs(
  vaccine_doses = config$history$vaccine_doses,
  vaccine_days = config$history$vaccine_days +
    config$history$immunity_onset_delay,
  import_days = config$history$import_days
)

# Run ABC ----------------------------------------------------------------------

simulate_from_priors <- function(priors) {
  seirMeasles::simulate(
    c(fixed_params, priors),
    manual_inputs = fixed_manual_inputs
  )
}

score_sim <- function(sim) {
  case_series |>
    filter(!is.na(observed_cases)) |>
    inner_join(sim, by = "day") |>
    mutate(day_score = abs(reported_cases - observed_cases)) |>
    pull(day_score) |>
    sum()
}

# draw parameters from priors
priors <- withr::with_seed(seed, seirMeasles::sample_priors(n_iter))

prior_grid <- priors |>
  as_tibble() |>
  bind_cols(fixed_params) |>
  mutate(sim = seq_len(n_iter))

sims <- seirMeasles::simulate_grid(
  prior_grid,
  fixed_manual_inputs,
  seed
)

results <- sims |>
  nest(data = -sim) |>
  mutate(score = map_dbl(data, score_sim)) |>
  # accept some fraction
  mutate(accept = percent_rank(score) < accept_p) |>
  # throw away the simulations themselves
  select(sim, accept) |>
  bind_cols(priors) |>
  # create a long tibble: each row is a parameter name, value, and accepted/not
  pivot_longer(!c(sim, accept))

ggplot(data = NULL, aes(value)) +
  facet_wrap(vars(name), scales = "free") +
  geom_density(data = results, color = "black", linetype = "dashed") +
  geom_density(data = filter(results, accept), color = "red") +
  cowplot::theme_cowplot()

ggsave("output/abc.png", bg = "white")

abc_table <- bind_rows(
  prior = results,
  posterior = filter(results, accept),
  .id = "dist"
) |>
  group_by(name, dist) |>
  summarize(
    q25 = quantile(value, 0.25),
    q50 = median(value),
    q75 = quantile(value, 0.75),
    .groups = "drop"
  )

write_csv(abc_table, "output/abc.csv")
