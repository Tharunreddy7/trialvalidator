test_that("check_date_order finds invalid date order", {

  ae <- data.frame(
    USUBJID = c("001", "002", "003"),
    AESTDT = as.Date(c("2026-01-01", "2026-03-10", "2026-04-01")),
    AEENDT = as.Date(c("2026-01-05", "2026-03-05", "2026-04-10"))
  )

  result <- check_date_order(ae, "AESTDT", "AEENDT")

  expect_equal(nrow(result), 1)
  expect_equal(result$USUBJID, "002")
})


test_that("check_date_order gives error when start variable is missing", {

  ae <- data.frame(
    USUBJID = c("001", "002"),
    AEENDT = as.Date(c("2026-01-05", "2026-03-05"))
  )

  expect_error(
    check_date_order(ae, "AESTDT", "AEENDT"),
    "Start date variable not found in the dataset."
  )

})


test_that("check_date_order gives error when end variable is missing", {

  ae <- data.frame(
    USUBJID = c("001", "002"),
    AESTDT = as.Date(c("2026-01-01", "2026-03-10"))
  )

  expect_error(
    check_date_order(ae, "AESTDT", "AEENDT"),
    "End date variable not found in the dataset."
  )

})
