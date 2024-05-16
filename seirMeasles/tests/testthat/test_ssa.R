with_seed_and_rng <- function(code, seed = 1234) {
  withr::with_seed(
    code = code,
    seed = seed,
    .rng_kind = "Mersenne-Twister",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )
}

test_that("ssa works for one species", {
  got <- with_seed_and_rng({
    seirMeasles::ssa(
      state = c(x = 0),
      transitions = list(c(x = 1)),
      rate_fun = function(state, t) 1,
      end_time = 100
    )
  })

  # 84 time steps, ending at time 100, ending with total 82 counts
  expect_equal(nrow(got), 98)
  expect_equal(got[98, "time"], c(time = 100))
  expect_equal(got[98, "x"], c(x = 96))
})

test_that("ssa works for multiple methods", {
  run_with_method <- function(method) {
    seirMeasles::ssa(
      state = c(x = 0),
      transitions = list(c(x = 1)),
      rate_fun = function(state, t) 1,
      end_time = 100,
      method = method
    )
  }

  expect_no_error(run_with_method("exact"))
  expect_no_error(run_with_method("adaptive"))
})

test_that("ssa catches rate change", {
  got <- with_seed_and_rng({
    seirMeasles::ssa(
      state = c(x = 0),
      transitions = list(c(x = 1)),
      rate_fun = function(state, t) {
        ifelse(t >= 1, 1e2, 0.0)
      },
      end_time = 2.0,
      checkpoints = c(1.0)
    )
  })

  # should start at zero
  expect_equal(got[1, "time"], c(time = 0.0))
  # then next time point should be at the checkpoint
  expect_equal(got[2, "time"], c(time = 1.0))
  # then we should get a bunch of counts
  expect_equal(got[nrow(got), "x"], c(x = 96))
})

test_that("starting from a time that isn't zero changes nothing", {
  start_0 <- with_seed_and_rng({
    seirMeasles::ssa(
      state = c(x = 0),
      transitions = list(c(x = 1)),
      rate_fun = function(state, t) {
        ifelse(t >= 1, 1e2, 0.0)
      },
      end_time = 2.0,
      checkpoints = c(1.0)
    )
  })

  start_100 <- with_seed_and_rng({
    seirMeasles::ssa(
      state = c(x = 0),
      transitions = list(c(x = 1)),
      rate_fun = function(state, t) {
        ifelse(t >= 101, 1e2, 0.0)
      },
      start_time = 100,
      end_time = 102.0,
      checkpoints = c(101.0)
    )
  })

  # times should be shifted by 100
  expect_equal(start_0[, "time"] + 100, start_100[, "time"])
  # but otherwise history should be identical
  expect_equal(start_0[, -1], start_100[, -1])
})

test_that("ssa() and adaptivetau::ssa.exact()", {
  state <- c(S = 99, I = 1, R = 0)

  transitions <- list(
    c(S = -1, I = 1),
    c(I = -1, R = 1)
  )

  rates <- function(state, params, t) {
    with(as.list(state), {
      c(
        S_to_I = 0.005 * S * I,
        I_to_R = I * 0.1
      )
    })
  }

  tf <- 100

  results_adaptivetau <- with_seed_and_rng({
    adaptivetau::ssa.exact(
      state,
      transitions,
      rates,
      params = NULL,
      tf = tf
    )
  })

  results_ssa <- with_seed_and_rng({
    seirMeasles::ssa(
      state,
      transitions,
      function(state, t) rates(state, NULL, t),
      tf
    )
  })

  expect_equal(results_ssa, results_adaptivetau)
})

test_that("ssa fail with wrong method", {
  expect_error(
    seirMeasles::ssa(
      state = c(x = 0),
      transitions = list(c(x = 1)),
      rate_fun = function(state, t) 1.0,
      end_time = 1.0,
      method = "nonexistent method"
    )
  )
})
