context("Convenience functions")


test_that("convenience functions work as expected", {
  
  # One-dimensional arrays should be flat (by default)
  z <- zeros(10, atleast_2d = FALSE)
  rn <- randn(10, atleast_2d = FALSE)
  ru <- rand(10, atleast_2d = FALSE)
  ri <- randi(imax = 100, 10, atleast_2d = FALSE)
  expect_that(dim(z), is_null())
  expect_that(dim(rn), is_null())
  expect_that(dim(ru), is_null())
  expect_that(dim(ri), is_null())
  
  # Two-dimensional arrays should NOT be flat (by default)
  z2 <- zeros(10, 3)
  rn2 <- randn(10, 3)
  ru2 <- rand(10, 3)
  ri2 <- randi(imax = 100, 10, 3)
  expect_that(dim(z2), equals(c(10, 3)))
  expect_that(dim(rn2), equals(c(10, 3)))
  expect_that(dim(ru2), equals(c(10, 3)))
  expect_that(dim(ri2), equals(c(10, 3)))
  
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
  
  # Arrays
  a1 <- fill(pi, 2, 2, 2)
  a2 <- pi * ones(2, 2, 2)
  a3 <- array(pi, dim = c(2, 2, 2))
  
  expect_that(a1, is_identical_to(a2))
  expect_that(a1, is_identical_to(a3))  
  expect_that(size(a1), equals(c(2, 2, 2)))  
  expect_that(size(a2), equals(c(2, 2, 2)))  
  
  # Resize a vector into an array
  x <- 1:8
  a <- resize(1:8, 2, 2, 2)
  expect_that(a, is_a("array"))
  expect_that(flatten(a), is_identical_to(x))
  
  # Meshgrid (equals)
  x <- linspace(0, 1, 3)
  y <- linspace(0, 1, 2)
  mg <- meshgrid(x, y)
  mx <- mat("0, 0.5, 1; 0, 0.5, 1")
  my <- mat("0, 0, 0; 1, 1, 1")
  expect_that(mg[[1]], is_identical_to(mx))
  expect_that(mg[[2]], is_identical_to(my))
  
  # Meshgrid (identical)
  x <- y <- seq(-5, 5, by = 0.1)
  mg <- meshgrid(x, y)
  z1 <- sin(mg[[1]]^2 + mg[[2]]^2) / (mg[[1]]^2 + mg[[2]]^2)
  z2 <- outer(x, y, function(x, y) sin(x^2 + y^2) / (x^2 + y^2))
  expect_that(z1, is_identical_to(z2))
  
  # Triangular matrices
  m1 <- mat("1, 1, 1, 0, 0; 
             1, 1, 1, 1, 0; 
             1, 1, 1, 1, 1")
  m2 <- mat("0, 0, 0, 0, 0; 
             1, 0, 0, 0, 0; 
             1, 1, 0, 0, 0")
  expect_that(tri(3, 5, k = 2), equals(m1, check.attributes = FALSE))
  expect_that(tri(3, 5, k = -1), equals(m2, check.attributes = FALSE))                       
  expect_that(tri(3, 5, diag = FALSE), equals(m2, check.attributes = FALSE))
  
  m3 <- matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)
  m4 <- mat(" 0,  0,  0; 
              4,  0,  0; 
              7,  8,  0; 
             10, 11, 12")
  expect_that(tril(m3, k = -1), equals(m4, check.attributes = FALSE))
  
})
