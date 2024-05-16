test_that("read config without error", {
  expect_no_error(read_params(
    system.file("params.yaml", package = "seirMeasles", mustWork = TRUE)
  ))
})

test_that("date_to_day errors on wrong input", {
  expect_error(date_to_day("2024-01-01", start_date = "1990-01-01"))
})

test_that("date to day works", {
  expect_equal(
    date_to_day(
      lubridate::ymd("2024-02-26"),
      start_date = lubridate::ymd("2024-02-01")
    ),
    26
  )
})

test_that("day_to_date work", {
  expect_equal(
    day_to_date(
      26,
      start_date = lubridate::ymd("2024-02-01")
    ),
    lubridate::ymd("2024-02-26")
  )
})

test_that("date_to_day and day_to_date are inverse", {
  n <- 100
  base_date <- lubridate::ymd("1990-01-01")

  withr::with_seed(1234, {
    start_dates <- base_date + lubridate::days(round(3000 * runif(n)))
    days <- round(3000 * runif(n))
  })

  dates <- purrr::map2(days, start_dates, day_to_date)
  inverse_days <- purrr::map2_dbl(dates, start_dates, date_to_day)

  expect_equal(days, inverse_days)
})
