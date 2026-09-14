test_that("check_duplicates returns duplicated records", {

  dm <- data.frame(
    USUBJID = c("001", "002", "002", "003"),
    AGE = c(45, 50, 50, 62)
  )

  result <- check_duplicates(dm, "USUBJID")

  expect_equal(nrow(result), 2)
  expect_equal(result$USUBJID, c("002", "002"))

})


test_that("check_duplicates gives an error for a missing variable", {

  dm <- data.frame(
    USUBJID = c("001", "002"),
    AGE = c(45, 50)
  )

  expect_error(
    check_duplicates(dm, "SEX"),
    "Variable not found in the dataset."
  )

})
