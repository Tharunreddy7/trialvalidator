test_that("check_allowed_values identifies invalid values", {

  dm <- data.frame(
    USUBJID = c("001", "002", "003", "004"),
    SEX = c("M", "F", "X", "U")
  )

  result <- check_allowed_values(
    dm,
    "SEX",
    c("M", "F", "U")
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$USUBJID, "003")
  expect_equal(result$SEX, "X")
})


test_that("check_allowed_values gives error for missing variable", {

  dm <- data.frame(
    USUBJID = c("001", "002")
  )

  expect_error(
    check_allowed_values(
      dm,
      "SEX",
      c("M", "F", "U")
    ),
    "Variable not found in the dataset."
  )

})
