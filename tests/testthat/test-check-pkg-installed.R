test_that("check_pkg_installed() works", {
  expect_snapshot(
    error = TRUE,
    x = check_pkg_installed("asdf")
  )
  expect_null(check_pkg_installed("targets"))
})
