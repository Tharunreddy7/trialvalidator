#' Check Duplicate Records
#'
#' Identifies duplicated values for a specified variable.
#'
#' @param data A data frame.
#' @param variable Character name of the variable to check.
#'
#' @return A data frame containing duplicated records.
#'
#' @export
check_duplicates <- function(data, variable) {

  if (!variable %in% names(data)) {
    stop("Variable not found in the dataset.")
  }

  duplicated_values <- duplicated(data[[variable]]) |
    duplicated(data[[variable]], fromLast = TRUE)

  data[duplicated_values, , drop = FALSE]
}
