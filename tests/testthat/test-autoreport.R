test_that("function unique_autoreport", {
  testthat::expect_equal(
    unique_autoreport(data = mtcars, entity = "gear"),
    c(4, 3, 5)
  )
})
