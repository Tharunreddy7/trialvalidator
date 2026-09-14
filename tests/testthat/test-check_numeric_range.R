test_that("check_numeric_range identifies values outside range", {

  dm <- data.frame(
    USUBJID = c("001", "002", "003", "004"),
    AGE = c(45, 17, 120, 62)
  )

  result <- check_numeric_range(
    dm,
    "AGE",
    18,
    100
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$USUBJID, c("002", "003"))
})


test_that("check_numeric_range gives error for missing variable", {

  dm <- data.frame(
    USUBJID = c("001", "002")
  )

  expect_error(
    check_numeric_range(
      dm,
      "AGE",
      18,
      100
    ),
    "Variable not found in the dataset."
  )

})
