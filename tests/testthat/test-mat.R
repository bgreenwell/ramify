context("Basic functionality")


test_that("character method functions as expected", {
  
  # Three versions of the same matrix
  m1 <- mat("1.5, 2.0, pi; 4, 5, 6; 7, 8, 9", eval = TRUE)
  m2 <- mat("3/2 2.0 pi  ; 4 5 6;    7:9 ", sep = " ", eval = TRUE)
  m3 <- matrix(c(1.5, 2, pi, 4:9), nrow = 3, byrow = TRUE)
  
  # Expectations
  expect_identical(m1, m2)
  expect_identical(m1, m3)
  expect_identical(m2, m3)
  
})


test_that("list method functions as expected", {
  
  z <- list(a = 1:10, b = 1:10, c = 1:10)
  m1 <- mat(z)
  m2 <- mat(z, rows = FALSE)
  m3 <- matrix(unlist(z), nrow = 3, byrow = TRUE)
  m4 <- do.call(rbind, z)
  m5 <- simplify2array(z)
  expect_identical(rownames(m1), names(z))  # check names
  expect_identical(colnames(m2), names(z))  # check names
  expect_identical(m1, t(m2))
  expect_identical(m1, m4)
  expect_identical(m2, m5)
  expect_equal(m1, m3, check.attributes = FALSE)
  
})


test_that("dmat functions as expected", {
  
  z <- list(a = 1:10, b = 1:10, c = 1:10)
  d1 <- dmat(z)
  d2 <- dmat(z, rows = FALSE)
  m1 <- mat(z)
  m2 <- mat(z, rows = FALSE)
  expect_identical(d1, as.data.frame(m1))
  expect_identical(d2, as.data.frame(m2))
  expect_identical(m1, t(m2))
  
})