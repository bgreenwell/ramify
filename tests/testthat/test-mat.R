context("Basic functionality")

test_that("mat and resize produce the correct matrices", {
  
  ## Should all produce a 3-by-3 matrix consisting of the integers 1-9 (by row).
  m1 <- mat("1, 2, 3; 4, 5, 6; 7, 8, 9", rows = TRUE)
  m2 <- resize(1:9, nrow = 3)
  m3 <- matrix(1:9, nrow = 3, byrow = TRUE)
  expect_that(m1, equals(m2))
  expect_that(m1, equals(m3))
  expect_that(m2, equals(m3))
  
  ## The function resize also works like transpose
  expect_that(resize(m1), equals(t(m1)))
  expect_that(resize(resize(m1)), equals(m1))
  
})

