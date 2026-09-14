#' Check Date Order
#'
#' Identifies records where the start date occurs after the end date.
#'
#' @param data A data frame.
#' @param start_var Character name of the start date variable.
#' @param end_var Character name of the end date variable.
#'
#' @return A data frame containing records with invalid date order.
#'
#' @export
check_date_order <- function(data, start_var, end_var) {

  if (!start_var %in% names(data)) {
    stop("Start date variable not found in the dataset.")
  }

  if (!end_var %in% names(data)) {
    stop("End date variable not found in the dataset.")
  }

  invalid <- !is.na(data[[start_var]]) &
    !is.na(data[[end_var]]) &
    data[[start_var]] > data[[end_var]]

  data[invalid, , drop = FALSE]
}
