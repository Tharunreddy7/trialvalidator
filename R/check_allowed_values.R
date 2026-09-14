#' Check Allowed Values
#'
#' Identifies records containing values that are not included
#' in a specified set of allowed values.
#'
#' @param data A data frame.
#' @param variable Character name of the variable to check.
#' @param allowed_values Vector of permitted values.
#'
#' @return A data frame containing records with invalid values.
#'
#' @export
check_allowed_values <- function(data, variable, allowed_values) {

  if (!variable %in% names(data)) {
    stop("Variable not found in the dataset.")
  }

  invalid <- !is.na(data[[variable]]) &
    !data[[variable]] %in% allowed_values

  data[invalid, , drop = FALSE]
}
