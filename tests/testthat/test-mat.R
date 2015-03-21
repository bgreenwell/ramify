context("Basic functionality")


test_that("character method functions as expected", {
  
  m1 <- mat("1.5, 2.0, pi; 4, 5, 6; 7, 8, 9")
  m2 <- mat("3/2 2.0 pi  ; 4 5 6;    7:9 ", sep = " ")
  m3 <- matrix(c(1.5, 2, pi, 4:9), nrow = 3, byrow = TRUE)
  expect_that(m1, is_identical_to(m2))
  expect_that(m1, is_identical_to(m3))
  expect_that(m2, is_identical_to(m3))
  
})


test_that("list method functions as expected", {
  
  z <- list(a = 1:10, b = 1:10, c = 1:10)
  m1 <- mat(z)
  m2 <- mat(z, rows = FALSE)
  m3 <- matrix(unlist(z), nrow = 3, byrow = TRUE)
  m4 <- do.call(rbind, z)
  m5 <- simplify2array(z)
  expect_that(rownames(m1), is_identical_to(names(z)))  # check names
  expect_that(colnames(m2), is_identical_to(names(z)))  # check names
  expect_that(m1, is_identical_to(t(m2)))
  expect_that(m1, is_identical_to(m4))
  expect_that(m2, is_identical_to(m5))
  expect_that(m1, equals(m3, check.attributes = FALSE))
  
})


test_that("dmat functions as expected", {
  
  z <- list(a = 1:10, b = 1:10, c = 1:10)
  d1 <- dmat(z)
  d2 <- dmat(z, rows = FALSE)
  m1 <- mat(z)
  m2 <- mat(z, rows = FALSE)
  expect_that(d1, is_identical_to(as.data.frame(m1)))
  expect_that(d2, is_identical_to(as.data.frame(m2)))
  expect_that(m1, is_identical_to(t(m2)))
  
})


test_that("convenience functions work as expected", {
  
  # Identity matrix
  expect_that(eye(3), is_identical_to(diag(3)))
  expect_that(eye(3, 5), is_identical_to(diag(1, 3, 5)))
  expect_that(eye(5, 3), is_identical_to(diag(1, 5, 3)))
  
  m1 <- matrix(c(0.1112850, 0.3735504, 0.7667462, 0.2012106), 2, 2)
  m2 <- matrix(c(0.6049852, 0.2716786), 1, 2)
  m3 <- matrix(c(0.6049852, 0.2716786), 2, 1)
  
  # Concatenate matrices
  expect_that(vcat(m1, m2), is_identical_to(rbind(m1, m2)))
  expect_that(hcat(m1, m3), is_identical_to(cbind(m1, m3)))
  
  # Flatten a matrix
  expect_that(flatten(mat("1:3; 4:6; 7:9")), is_identical_to(1:9))
  expect_that(flatten(mat("1:3; 4:6; 7:9", rows = FALSE), across = "columns"), 
              is_identical_to(1:9))
  
  # Matrix inverse
  expect_that(inv(m1), is_identical_to(solve(m1)))
  expect_that(inv(3 * eye(3)), is_identical_to(eye(3) / 3))
  
})

