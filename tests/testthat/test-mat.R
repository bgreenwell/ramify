context("Basic functionality")

test_that("character method works as expected", {
  
  m1 <- mat("1.5, 2.0, pi; 4, 5, 6; 7, 8, 9")
  m2 <- mat("3/2 2.0 pi  ; 4 5 6;    7:9 ", sep = " ")
  m3 <- matrix(c(1.5, 2, pi, 4:9), nrow = 3, byrow = TRUE)
  expect_identical(m1, m2)
  expect_identical(m1, m3)
  expect_identical(m2, m3)
  
})

test_that("list method works as expected", {
  
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

test_that("dmat works as expected", {
  
  z <- list(a = 1:10, b = 1:10, c = 1:10)
  d1 <- dmat(z)
  d2 <- dmat(z, rows = FALSE)
  m1 <- mat(z)
  m2 <- mat(z, rows = FALSE)
  expect_identical(d1, as.data.frame(m1))
  expect_identical(d2, as.data.frame(m2))
  expect_identical(m1, t(m2))
  
})

test_that("bmat works as expected", {
  
  # Using base R
  A1 <- matrix(c(1, 2, 5, 6), nrow = 2, byrow = TRUE)
  A2 <- matrix(c(3, 4, 7, 8), nrow = 2, byrow = TRUE)
  A3 <- matrix(c(9, 10, 11, 12), nrow = 1)
  A <- rbind(cbind(A1, A2), A3)
  
  # Using ramify
  B1 <- bmat("1, 2; 5, 6")
  B2 <- bmat("3, 4; 7, 8")
  B3 <- bmat("9, 10, 11, 12")
  B <- bmat("B1, B2; B3")
  
  expect_equal(A1, B1)
  expect_equal(A2, B2)
  expect_equal(A3, B3)
  expect_equal(A, B)
  
})

