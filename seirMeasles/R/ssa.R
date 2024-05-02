#' Stochastic simulation algorithm (SSA)
#'
#' Wrap [adaptivetau::ssa.exact()] or [adaptivetau::ssa.adaptivetau()], allowing
#' for checkpoints and specific start times
#'
#' @param state named vector
#' @param transitions list of transitions passed to
#'   [adaptivetau::ssa.adaptivetau()]
#' @param rate_fun function of state and time, returning a vector of rates
#' @param end_time end of simulation
#' @param start_time default: 0
#' @param checkpoints vector of times; ensure rate_fun is re-evaluated at
#'   these times
#' @param method one of `"exact"` (i.e., Gillespie; uses
#'   [adaptivetau::ssa.exact()]) or `"adaptive"` (uses
#'   [adaptivetau::ssa.adaptivetau()]). Default: `"exact"`
#' @param ... additional parameters passed to [adaptivetau::ssa.adaptivetau()]
#'
#' @return list of lists, one per transition. each list has elements `time` and
#'   `state`
#' @export
ssa <- function(
    state,
    transitions,
    rate_fun,
    end_time,
    start_time = 0,
    checkpoints = NULL,
    method = "exact",
    ...) {
  # validate inputs -----------------------------------------------------------
  n_species <- length(state)
  n_reactions <- nrow(transitions)

  # all species in transitions should be in the state
  state_names <- names(state)
  if ("time" %in% state_names) warning("Potential name collision")
  missing_names <- transitions |>
    purrr::map(names) |>
    purrr::list_c() |>
    setdiff(state_names)

  if (length(missing_names) > 0) {
    stop(
      "species names in transitions but not in state: ",
      do.call(paste, as.list(missing_names))
    )
  }

  # if there are checkpoints, make sure they are between the start and end
  # times. and sorted
  stopifnot(all(checkpoints > start_time))
  stopifnot(all(checkpoints <= end_time))
  if (is.unsorted(checkpoints)) stop("Checkpoint times must be sorted")

  # set up --------------------------------------------------------------------

  # check method
  fun <- switch(method,
    "exact" = adaptivetau::ssa.exact,
    "adaptive" = adaptivetau::ssa.adaptivetau,
    stop(method, " is not 'exact' or 'adaptive'")
  )

  # set up history with the first state
  history <- c(time = start_time, state) |>
    matrix(nrow = 1, dimnames = list(NULL, c("time", state_names)))

  # run each "step". a step is a chunk of time between two checkpoints --------

  times <- c(start_time, checkpoints, end_time)
  for (i in seq_len(length(times) - 1)) {
    step_start <- times[i]
    step_end <- times[i + 1]

    step_history <- fun(
      init.values = state,
      transitions = transitions,
      rateFunc = function(state, pars, t) rate_fun(state, t + step_start),
      params = NULL,
      tf = step_end - step_start,
      ...
    )

    stopifnot(all(colnames(step_history) == c("time", state_names)))

    # adjust from within-step time to global time
    step_history[, "time"] <- step_history[, "time"] + step_start
    # drop the first line in each step's history, which is just a duplication of
    # the last line in the previous step's history
    step_history <- step_history[-1, , drop = FALSE]
    history <- rbind(history, step_history)

    # starting state is the ending state of the last step
    state <- step_history[nrow(step_history), -1]
  }

  history
}
