#' Sample the prior distributions
#'
#' @details
#' r0 ~ pert(min = 10, mode = 20, max = 60, shape = 5)
#' int_eff ~ pert(min = 0, mode = .2, max = .8, shape = 5)
#'
#' @param n Number of samples to draw
#'
#' @return Named list of parameter values
#'
#' @export
sample_priors <- function(n) {
  list(
    # alternative r0 runif(n, 10, 26)
    r_0 = mc2d::rpert(n, min = 5, mode = 12, max = 45, shape = 5),
    # alternative r0_eff runif(n, 0, 0.5)
    int_eff = mc2d::rpert(n, min = 0, mode = .2, max = .8, shape = 5)
  )
}
