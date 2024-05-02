#' SEIR rates
#'
#' @param x state vector
#' @param parameters parameter grid row
#' @param t simulation time
#'
#' @return vector of rates
#' @export
rates <- function(x, parameters, t) {
  S <- x[paste0("s", 1:5)]
  SV <- x[paste0("sv", 1:5)]
  E1 <- x[paste0("e1_", 1:5)]
  E2 <- x[paste0("e2_", 1:5)]
  I <- x[paste0("i", 1:5)]
  R <- x[paste0("r", 1:5)]

  pre_reported_cases <- x["pre_reported_cases"]
  reported_cases <- x["reported_cases"]
  cases <- x["cases"]

  N <- sum(S) + sum(SV) + sum(E1) + sum(E2) + sum(I) + sum(R)

  with(as.list(parameters), {
    # gamma (recovery rate) is inverse of infectious period
    gam <- 1.0 / infectious_period

    # infection rates use "who acquires infection from whom" matrix
    # rate of infection in group i = beta * sum_j S_i * I_j * WAIFW_ij
    # ie element-wise product of S vector with {WAIFW times I column vector}
    # all entries equal for homogenous mixing
    beta <- r_0 * gam
    waifw <- matrix(data = 1.0, nrow = 5, ncol = 5)
    s_infection_rates <- beta * S * (waifw %*% I) / N
    sv_infection_rates <- beta * SV * (waifw %*% I) / N
    stopifnot(length(s_infection_rates) == 5)
    stopifnot(length(sv_infection_rates) == 5)

    # from E1 to E2
    # note: relevant rate is half of inverse of *total* latent period
    sigma <- 1.0 / latent_period
    exposure_transition_rates <- 2.0 * sigma * E1

    # from pre-infectious to infectious (among exposed)
    infection_onset_rates <- 2.0 * sigma * E2

    # recovery in the infected in each group
    # note that the dynamic value of gamma does *not* affect beta, which is
    # computed based on R0 and the baseline gamma
    if (is.na(int_day) || t < int_day) {
      gam_dynamic <- gam
    } else if (t >= int_day) {
      gam_dynamic <- gam / (1.0 - int_eff)
    } else {
      stop("Bad int_day")
    }
    recovery_rates <- gam_dynamic * I

    c(
      s_infection_rates,
      sv_infection_rates,
      exposure_transition_rates,
      infection_onset_rates,
      recovery_rates,
      # case goes from pre reported to reported
      (1.0 / ascertainment_delay) * pre_reported_cases
    )
  })
}
