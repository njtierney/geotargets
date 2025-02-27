test_that("check_user_resources works", {
  expect_snapshot(
    error = TRUE,
    check_user_resources(resources = c("custom_format" = 1))
  )
  expect_silent(
    check_user_resources(resources = c("anything else" = 1))
  )
})
