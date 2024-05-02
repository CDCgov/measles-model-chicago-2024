test_that("snapshot sample prior", {
  # get results to 2 decimal places
  got <- withr::with_seed(1234, sample_priors(3)) |>
    purrr::map(\(x) round(x, 2))

  expect_equal(
    got,
    list(
      r_0 = c(8.58, 18.08, 26.60),
      int_eff = c(0.02, 0.33, 0.35)
    )
  )
})
