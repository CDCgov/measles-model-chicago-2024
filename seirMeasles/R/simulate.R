#' Generate imports and vaccine inputs
#'
#' @param vaccine_doses vector of vaccine counts
#' @param vaccine_days integer vector of time points, same length as
#'  `vaccine_doses`
#' @param import_days integer vector of time points
#'
#' @return tibble, each row is an event (import or vaccination day)
#'
#' @export
generate_inputs <- function(
    vaccine_doses = NULL,
    vaccine_days = NULL,
    import_days = NULL) {
  # there must be at least one import
  stopifnot(length(import_days) > 0)
  # there can be zero vaccine dates, but inputs must match length
  stopifnot(length(vaccine_doses) == length(vaccine_days))

  # start with imports
  out <- dplyr::tibble(
    day = import_days,
    doses = 0,
    imports_1 = 0,
    imports_2 = 0,
    # N.B.: A single import into pop 3 is hard-coded
    imports_3 = 1,
    imports_4 = 0,
    imports_5 = 0
  )

  # add vaccinations, if any
  if (length(vaccine_days) > 0) {
    out <- out |>
      bind_rows(dplyr::tibble(
        day = vaccine_days,
        doses = vaccine_doses,
        imports_1 = 0,
        imports_2 = 0,
        imports_3 = 0,
        imports_4 = 0,
        imports_5 = 0
      ))
  }

  # return time-sorted output
  out |>
    arrange(pick("day"))
}

#' Do importations
#'
#' @param x simulation compartment state
#' @param imports vector of importations, with length equal to number of
#'   populations
#'
#' @return state vector
do_importations <- function(x, imports) {
  stopifnot(length(imports) == 5)

  i_names <- paste0("i", 1:5)
  x[i_names] <- x[i_names] + imports

  # note that cases normally increment on E->I transition, so need to
  # trigger these manually with importation
  x["pre_reported_cases"] <- x["pre_reported_cases"] + sum(imports)
  x["cases"] <- x["cases"] + sum(imports)

  x
}

#' Do vaccinations
#'
#' @param x simulation compartment state
#' @param doses integer number of dose counts
#' @param ve non-infant vaccine effectiveness
#' @param ve_infant infant vaccine effectiveness
#' @param prop_vax_to_se proportion of vaccines alloted to S & E compartments
#'   (ie not R)
#'
#' @return state vector
do_vaccinations <- function(x, doses, ve, ve_infant, prop_vax_to_se) {
  stopifnot(is.finite(doses))
  stopifnot(0 <= ve && ve <= 1.0)
  stopifnot(0 <= ve_infant && ve_infant <= 1.0)

  # get number of people in S and E compartments
  se <- c(
    # everyone who is eligible, unvaccinated, and doesn't have documentation
    x[c(paste0("s", 2:4), paste0("e1_", 2:4), paste0("e2_", 2:4))]
  )

  # total doses that go to S & E people
  se_doses_total <- doses * prop_vax_to_se

  # doses that go to each S & E compartment: take all the doses and divide
  # them proportionally among the compartments
  # (or, at most, the number of people in each compartment)
  se_doses <- round(pmin(se_doses_total * se / sum(se), se))

  # move everyone who has successful vaccination
  # assume all E vaccinations are failures
  x["s2"] <- x["s2"] - se_doses["s2"]
  x["s3"] <- x["s3"] - se_doses["s3"]
  x["s4"] <- x["s4"] - se_doses["s4"]

  x["sv2"] <- x["sv2"] + round(se_doses["s2"] * (1 - ve_infant))
  x["sv3"] <- x["sv3"] + round(se_doses["s3"] * (1 - ve))
  x["sv4"] <- x["sv4"] + round(se_doses["s4"] * (1 - ve))

  x["r2"] <- x["r2"] + round(se_doses["s2"] * ve_infant)
  x["r3"] <- x["r3"] + round(se_doses["s3"] * ve)
  x["r4"] <- x["r4"] + round(se_doses["s4"] * ve)

  x
}

#' Run one simulation
#'
#' @param params named vector of parameters
#' @param manual_inputs tibble as returned by `generate_inputs()`
#' @param sim_length duration of simulation in days (default: 365)
#' @param ssa_method passed to `method` argument of [seirMeasles::ssa()]
#' @param ... additional arguments passed to [seirMeasles::ssa()]
#'
#' @return tibble
#'
#' @export
simulate <- function(
    params,
    manual_inputs,
    sim_length = 365,
    ssa_method = "exact",
    ...) {
  # add in the "dummy" transition, to ensure simulation changes rates at time of
  # "intervention"
  stopifnot("int_day" %in% names(params))

  if (!is.na(params[["int_day"]])) {
    dummy_transition <- dplyr::tibble(
      day = params[["int_day"]],
      doses = 0,
      imports_1 = 0,
      imports_2 = 0,
      imports_3 = 0,
      imports_4 = 0,
      imports_5 = 0
    )

    manual_inputs <- manual_inputs |>
      bind_rows(dummy_transition) |>
      arrange(pick("day"))
  }

  if (is.unsorted(manual_inputs$day)) stop("Manual inputs are not time-ordered")
  stopifnot(max(manual_inputs$day) < sim_length)

  # initialize population
  pop_1 <- params[["pop_1"]]
  pop_2 <- params[["pop_2"]]
  pop_3 <- params[["pop_3"]]
  pop_4 <- params[["pop_4"]]
  pop_5 <- params[["pop_5"]]

  prop_s_1 <- params[["prop_s_1"]]
  prop_s_2 <- params[["prop_s_2"]]
  prop_s_3 <- params[["prop_s_3"]]
  prop_s_4 <- params[["prop_s_4"]]
  prop_s_5 <- params[["prop_s_5"]]

  s1_0 <- floor(prop_s_1 * pop_1)
  s2_0 <- floor(prop_s_2 * pop_2)
  s3_0 <- floor(prop_s_3 * pop_3)
  s4_0 <- floor(prop_s_4 * pop_4)
  s5_0 <- floor(prop_s_5 * pop_5)

  sv1_0 <- sv2_0 <- sv3_0 <- sv4_0 <- sv5_0 <- 0

  # labeled with compartment label first, age group second
  e1_1_0 <- e1_2_0 <- e1_3_0 <- e1_4_0 <- e1_5_0 <- 0
  e2_1_0 <- e2_2_0 <- e2_3_0 <- e2_4_0 <- e2_5_0 <- 0

  i1_0 <- params[["i0_1"]]
  i2_0 <- params[["i0_2"]]
  i3_0 <- params[["i0_3"]]
  i4_0 <- params[["i0_4"]]
  i5_0 <- params[["i0_5"]]

  r1_0 <- pop_1 - s1_0 - e1_1_0 - e2_1_0 - i1_0
  r2_0 <- pop_2 - s2_0 - sv2_0 - e1_2_0 - e2_2_0 - i2_0
  r3_0 <- pop_3 - s3_0 - sv3_0 - e1_3_0 - e2_3_0 - i3_0
  r4_0 <- pop_4 - s4_0 - sv4_0 - e1_4_0 - e2_4_0 - i4_0
  r5_0 <- pop_5 - s5_0 - e1_5_0 - e2_5_0 - i5_0

  x <- c(
    s1 = s1_0, sv1 = sv1_0, e1_1 = e1_1_0, e2_1 = e2_1_0, i1 = i1_0, r1 = r1_0,
    s2 = s2_0, sv2 = sv2_0, e1_2 = e1_2_0, e2_2 = e2_2_0, i2 = i2_0, r2 = r2_0,
    s3 = s3_0, sv3 = sv3_0, e1_3 = e1_3_0, e2_3 = e2_3_0, i3 = i3_0, r3 = r3_0,
    s4 = s4_0, sv4 = sv4_0, e1_4 = e1_4_0, e2_4 = e2_4_0, i4 = i4_0, r4 = r4_0,
    s5 = s5_0, sv5 = sv5_0, e1_5 = e1_5_0, e2_5 = e2_5_0, i5 = i5_0, r5 = r5_0,
    pre_reported_cases = 0,
    reported_cases = 0,
    cases = 0
  )

  compartments <- names(x)

  output <- array(dim = c(nrow(manual_inputs), length(x)))
  colnames(output) <- compartments
  output[1, ] <- x

  # derived parameter: proportion of doses in vaccinations that will go to S
  # and E compartments. assume this is fixed. first, get proportion of vaccine-
  # eligible populations that were susceptible at the start of the simulation
  prop_s <- (s2_0 + s3_0 + s4_0) / (pop_2 + pop_3 + pop_4)
  # proportion of new doses that will go to S and E compartments
  prop_vax_to_se <- prop_s / (1.0 - params[["documented_vax_coverage"]])

  # assume that simulation starts from time zero and runs until the first
  # "manual" event
  history <- ssa(
    x,
    transitions,
    function(state, t) rates(state, params, t),
    start_time = 0,
    end_time = manual_inputs$day[1],
    method = ssa_method,
    ...
  )

  output <- history
  # get last row, drop first column (time)
  x <- history[nrow(history), -1]

  # iterate over all the manual inputs
  # for each manual input:
  # - do importations, if any
  # - do vaccinations, if any
  # - take the next time step
  # - record results of that time step
  for (j in seq_len(nrow(manual_inputs))) {
    # imports
    imports <- manual_inputs |>
      dplyr::slice(j) |>
      select(dplyr::starts_with("imports_")) |>
      unlist()

    x <- do_importations(x, imports)

    # vaccination
    x <- do_vaccinations(
      x,
      doses = manual_inputs$doses[j],
      ve = params[["ve"]],
      ve_infant = params[["ve_infant"]],
      prop_vax_to_se = prop_vax_to_se
    )

    # confirm that x has kept its order
    stopifnot(all(names(x) == compartments))

    # prepare next time step
    if (j < nrow(manual_inputs)) {
      next_time <- manual_inputs$day[j + 1]
    } else if (j == nrow(manual_inputs)) {
      next_time <- sim_length
    } else {
      stop("Unexpected manual input location")
    }

    # advance model
    history <- ssa(
      x,
      transitions,
      function(state, t) rates(state, params, t),
      start_time = manual_inputs$day[j],
      end_time = next_time,
      method = ssa_method,
      ...
    )

    # record transitions from that model step
    output <- rbind(output, history)
    x <- history[nrow(history), -1]
  }

  output_df <- output |>
    as.data.frame() |>
    # remove duplicate rows that emerge from starting and stopping the
    # chunks
    dplyr::distinct() |>
    # simplify the outputs by taking only the final state for each day
    dplyr::rename("day" = "time") |>
    group_by(pick("day")) |>
    mutate(dplyr::across(dplyr::any_of("day"), ceiling)) |>
    group_by(pick("day")) |>
    dplyr::slice(dplyr::n()) |>
    ungroup()

  # interpolate each thing
  t_out <- 0:sim_length
  output_names <- setdiff(names(output_df), "day")
  interp <- purrr::map(
    output_names,
    function(nm) {
      y <- output_df[[nm]]
      stats::approx(
        x = output_df$day,
        y = y,
        xout = t_out,
        method = "constant",
        yright = dplyr::last(y)
      )$y
    }
  ) |>
    purrr::set_names(output_names) |>
    dplyr::as_tibble() |>
    mutate(day = t_out)

  # add parameters -- this isn't an efficient way to do this, but it's
  # also not efficient to be tracking parameters for each line
  for (nm in names(params)) {
    interp[[nm]] <- params[[nm]]
  }

  interp
}

#' Run a grid of simulations, using a config setup
#'
#' @param config list
#' @param ... Additional parameters passed to [simulate_grid()]
#'
#' @export
simulate_config <- function(config, ...) {
  parameter_grid <- expand.grid(c(
    list(sim = 1:(config$n_reps)),
    config$params
  ))

  manual_inputs <- with(config$history, {
    generate_inputs(
      vaccine_doses = vaccine_doses,
      vaccine_days = vaccine_days + immunity_onset_delay,
      import_days = import_days
    )
  })

  simulate_grid(
    parameter_grid,
    manual_inputs = manual_inputs,
    ...
  )
}

#' Run simulations on a grid of parameters
#'
#' @param parameter_grid data.frame of values. Each row is interpreted as a
#'   named list of scalar parameters
#' @param manual_inputs tibble of importations and vaccinations as produced
#'   by `generate_inputs()`
#' @param seed random number seed
#' @param ... Additional parameters passed to [`simulate()`]
#'
#' @return tibble of simulation results and parameters
#'
#' @export
simulate_grid <- function(
    parameter_grid,
    manual_inputs,
    seed,
    ...) {
  furrr::future_map_dfr(
    seq_len(nrow(parameter_grid)),
    function(i) {
      seirMeasles::simulate(parameter_grid[i, ], manual_inputs, ...)
    },
    .options = furrr::furrr_options(seed = seed)
  )
}
