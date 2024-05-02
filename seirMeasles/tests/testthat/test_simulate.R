test_that("end to end test", {
  seed <- 13
  # number of simulations
  n_reps <- 10

  config <- read_params(test_path("testdata", "params_test.yaml"))

  parameter_grid <- expand.grid(c(
    list(sim = 1:n_reps),
    config$params
  ))

  manual_inputs <- generate_inputs(
    import_days = config$history$import_days,
    vaccine_doses = config$history$vaccine_doses,
    vaccine_days = config$history$vaccine_days +
      config$history$immunity_onset_delay
  )

  output <- simulate_grid(
    parameter_grid,
    manual_inputs = manual_inputs,
    seed = seed
  )

  snapshot <- arrow::read_parquet(test_path("testdata", "snapshot.parquet"))

  # some ad hoc changes to account for updates to the input
  snapshot <- snapshot |>
    mutate(
      latent_period = 2.0 / thet,
      ascertainment_delay = 1.0 / phi,
      infectious_period = 1.0 / gam
    ) |>
    select(names(output))

  expect_equal(output, snapshot)
})

test_that("parallel simulation runs without error", {
  # only run this test if multicore is supported
  skip_if_not(future::supportsMulticore())

  seed <- 13
  # number of simulations
  n_reps <- 4

  config <- read_params(test_path("testdata", "params_test.yaml"))

  parameter_grid <- expand.grid(c(
    list(sim = 1:n_reps),
    config$params
  ))

  manual_inputs <- generate_inputs(
    import_days = config$history$import_days,
    vaccine_doses = config$history$vaccine_doses,
    vaccine_days = config$history$vaccine_days +
      config$history$immunity_onset_delay
  )

  oplan <- future::plan(future::multicore(workers = 2))

  expect_no_error({
    simulate_grid(
      parameter_grid,
      manual_inputs = manual_inputs,
      seed = seed
    )
  })

  future::plan(oplan)
})

test_that("do_importations() works", {
  x <- c(
    i1 = 1, i2 = 2, i3 = 3, i4 = 4, i5 = 5,
    r1 = 0,
    pre_reported_cases = 0, cases = 0
  )
  imports <- c(0, 0, 1, 0, 0)
  out <- do_importations(x, imports)
  expect_equal(
    out,
    c(
      i1 = 1, i2 = 2, i3 = 4, i4 = 4, i5 = 5,
      r1 = 0,
      pre_reported_cases = 1, cases = 1
    )
  )
})

test_that("do_importations() errors on wrong number of imports", {
  x <- c(
    i1 = 1, i2 = 2, i3 = 3, i4 = 4, i5 = 5,
    r1 = 0,
    pre_reported_cases = 0, cases = 0
  )
  imports <- c(0, 1)
  expect_error({
    do_importations(x, imports)
  })
})

test_that("do_vaccinations() works", {
  x <- c(
    s2 = 100, s3 = 50, s4 = 0,
    e1_2 = 0, e1_3 = 0, e1_4 = 0,
    e2_2 = 0, e2_3 = 0, e2_4 = 0,
    sv2 = 0, sv3 = 0, sv4 = 0,
    r2 = 0, r3 = 0, r4 = 0
  )

  out <- do_vaccinations(
    x,
    doses = 40,
    prop_vax_to_se = 0.75,
    ve = 0.9,
    ve_infant = 0.8
  )

  # of the 40 doses, 30 go to S & E compartments
  # no E, so all go to S
  # 20 go to pop 2, and 10 to pop 3, by population size
  # 20*0.8=16 successes (and 4 failures) in pop 2 (infants)
  # 10*0.9=9 successes (and 1 failure) in pop 3

  expect_equal(
    out,
    c(
      s2 = 80, s3 = 40, s4 = 0,
      e1_2 = 0, e1_3 = 0, e1_4 = 0,
      e2_2 = 0, e2_3 = 0, e2_4 = 0,
      sv2 = 4, sv3 = 1, sv4 = 0,
      r2 = 16, r3 = 9, r4 = 0
    )
  )
})

test_that("do_vaccinations() fails on bad input", {
  x <- c(s1 = 0)

  expect_error({
    do_vaccinations(
      x,
      doses = NA, ve = 0.9, ve_infant = 0.8, prop_vax_to_se = 0.5
    )
  })

  expect_error({
    do_vaccinations(
      x,
      doses = NA, ve = -1.0, ve_infant = 0.8, prop_vax_to_se = 0.5
    )
  })

  expect_error({
    do_vaccinations(
      x,
      doses = NA, ve = 0.9, ve_infant = 8.0, prop_vax_to_se = 0.5
    )
  })
})
