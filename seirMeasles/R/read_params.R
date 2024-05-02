#' Read simulation config file
#'
#' Config file must have:
#' - `$history`
#'   - `$start_date`
#'   - `$int_day`
#'   - `$import_days`
#'   - `$vaccine_days`
#'
#' @param path to yaml
#'
#' @return list of params
#' @export
read_params <- function(path) {
  x <- yaml::read_yaml(path)

  # int_day can have NULLs; replace with NAs before parsing dates
  x$params$int_day <- replace_null(x$params$int_day)

  # parse start date
  x$history$start_date <- lubridate::ymd(x$history$start_date)

  str_to_day <- function(s) {
    date_to_day(lubridate::ymd(s), start_date = x$history$start_date)
  }

  # convert date strings to integer days
  x$history <- purrr::modify_at(
    x$history,
    c("import_days", "vaccine_days"),
    str_to_day
  )

  stopifnot(length(x$history$vaccine_days) == length(x$history$vaccine_doses))

  x$params <- purrr::modify_at(x$params, "int_day", str_to_day)

  x
}

#' Date to integer day index
#'
#' @param dt date
#' @param start_date day 1
#'
#' @return integer index
#'
#' @export
date_to_day <- function(dt, start_date) {
  stopifnot(lubridate::is.Date(dt))
  (dt - start_date) / lubridate::ddays(1) + 1
}

#' Integer day index to date
#'
#' @param day integer day
#' @param start_date date of day 1
#'
#' @return date
#'
#' @export
day_to_date <- function(day, start_date) {
  start_date + lubridate::days(day - 1)
}

#' Replace NULL values by NA
#'
#' @param lst a list
#'
#' @return vector with `NULL` values replaced by `NA`
replace_null <- function(lst) {
  purrr::map(lst, function(elt) {
    if (is.null(elt)) {
      NA
    } else {
      elt
    }
  }) |>
    unlist()
}
