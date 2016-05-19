context("Utility functions")

test_that("utility functions work correctly", {

  x <- 1:10
  x.dots <- add_dots(x, pos = 3)
  expect_identical(x.dots, c("1", "2", "...", "10"))

  m1 <- matrix(1:4, nrow = 2)
  m2 <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2)
  m3 <- matrix(c(letters[1:4]), nrow = 2)
  expect_identical(desc_mat(m1), "2 x 2 matrix of integers:")
  expect_identical(desc_mat(m2), "2 x 2 matrix of doubles:")
  expect_identical(desc_mat(m3), "2 x 2 matrix of characters:")

})
