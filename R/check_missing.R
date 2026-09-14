#' Count Missing Values
#'
#' Counts the number of missing values in a specified variable.
#'
#' @param data A data frame.
#' @param variable Character name of the variable to check.
#'
#' @return Number of missing values.
#'
#' @export
check_missing <- function(data, variable) {

  if (!variable %in% names(data)) {
    stop("Variable not found in the dataset.")
  }

  sum(is.na(data[[variable]]))
}
