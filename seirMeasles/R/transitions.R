#' SEIR transitions
#'
#' See [seirMeasles::ssa()] for details on transition matrix setup
#'
#' @export
transitions <- list(
  # infection of susceptible (one transition per group)
  c(s1 = -1, e1_1 = 1),
  c(s2 = -1, e1_2 = 1),
  c(s3 = -1, e1_3 = 1),
  c(s4 = -1, e1_4 = 1),
  c(s5 = -1, e1_5 = 1),

  # infection of vaccinated but susceptible
  # note that populations 1 and 5 are ineligible, so those transitions should
  # never happen
  c(sv1 = -1, e1_1 = 1),
  c(sv2 = -1, e1_2 = 1),
  c(sv3 = -1, e1_3 = 1),
  c(sv4 = -1, e1_4 = 1),
  c(sv5 = -1, e1_5 = 1),

  # from latency 1 to latency 2
  c(e1_1 = -1, e2_1 = 1),
  c(e1_2 = -1, e2_2 = 1),
  c(e1_3 = -1, e2_3 = 1),
  c(e1_4 = -1, e2_4 = 1),
  c(e1_5 = -1, e2_5 = 1),

  # from latency 2 to active infection (also tallying infections_)
  c(e2_1 = -1, i1 = 1, pre_reported_cases = 1, cases = 1),
  c(e2_2 = -1, i2 = 1, pre_reported_cases = 1, cases = 1),
  c(e2_3 = -1, i3 = 1, pre_reported_cases = 1, cases = 1),
  c(e2_4 = -1, i4 = 1, pre_reported_cases = 1, cases = 1),
  c(e2_5 = -1, i5 = 1, pre_reported_cases = 1, cases = 1),

  # recovery from infection
  c(i1 = -1, r1 = 1),
  c(i2 = -1, r2 = 1),
  c(i3 = -1, r3 = 1),
  c(i4 = -1, r4 = 1),
  c(i5 = -1, r5 = 1),

  # case ascertainment
  c(reported_cases = 1, pre_reported_cases = -1)
)
