#' Sample the prior distributions
#'
#' @param n Number of samples to draw
#'
#' @return Named list of parameter values
#'
#' @export
sample_priors <- function(n) {
  list(
    r_0 = mc2d::rpert(n, min = 5, mode = 12, max = 45, shape = 5),
    int_eff = mc2d::rpert(n, min = 0, mode = .2, max = .8, shape = 5)
  )
}
