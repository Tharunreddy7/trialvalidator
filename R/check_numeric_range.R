#' Check Numeric Range
#'
#' Identifies records where values fall outside a specified
#' numeric range.
#'
#' @param data A data frame.
#' @param variable Character name of the numeric variable to check.
#' @param min_value Minimum permitted value.
#' @param max_value Maximum permitted value.
#'
#' @return A data frame containing records outside the allowed range.
#'
#' @export
check_numeric_range <- function(data,
                                variable,
                                min_value,
                                max_value) {

  if (!variable %in% names(data)) {
    stop("Variable not found in the dataset.")
  }

  invalid <- !is.na(data[[variable]]) &
    (data[[variable]] < min_value |
       data[[variable]] > max_value)

  data[invalid, , drop = FALSE]
}
