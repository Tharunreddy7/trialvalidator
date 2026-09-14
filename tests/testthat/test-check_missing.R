test_that("check_missing counts missing values correctly", {

  dm <- data.frame(
    USUBJID = c("001", "002", "003"),
    AGE = c(45, NA, 62)
  )

  result <- check_missing(dm, "AGE")

  expect_equal(result, 1)

})


test_that("check_missing gives an error for a missing variable", {

  dm <- data.frame(
    USUBJID = c("001", "002"),
    AGE = c(45, 50)
  )

  expect_error(
    check_missing(dm, "SEX"),
    "Variable not found in the dataset."
  )

})
