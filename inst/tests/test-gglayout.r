context("gglayout")

test_that("gglayout.matrix", {
  # ok
  m <- matrix(c(
    1, 1, 2,
    1, 1, 2,
    3, 3, 2
    ), 3, byrow=TRUE)
  expect_equal(class(gglayout(m)), "gglayout")
  
  # ok if 0 or NA is not rect
  m <- matrix(c(
    1, 1, 0,
    1, 1, 0,
    0, 0, 2
    ), 3, byrow=TRUE)
  expect_equal(class(gglayout(m)), "gglayout")
  
  # bad for 1
  m <- matrix(c(
    1, 1, 2,
    1, 1, 2,
    3, 3, 1
    ), 3, byrow=TRUE)
  expect_error(gglayout(m), "matrix includes non-rectanglar submatrix.*")
  
  # bad for 2
  m <- matrix(c(
    1, 2, 2,
    1, 2, 2,
    3, 3, 2
    ), 3, byrow=TRUE)
  expect_error(gglayout(m), "matrix includes non-rectanglar submatrix.*")
  
  # ok for n x m matrix
  m <- matrix(c(
    1, 2, 2, 2,
    1, 2, 2, 2,
    3, 3, 3, 3
    ), 3, byrow=TRUE)
  expect_equal(class(gglayout(m)), "gglayout")
  
  # bad for n x m matrix
  m <- matrix(c(
    1, 1, 2, 2,
    1, 2, 2, 2,
    3, 3, 3, 3
    ), 3, byrow=TRUE)
  expect_error(gglayout(m), "matrix includes non-rectanglar submatrix.*")
  
  # bad for donut type
  m <- matrix(c(
    1, 1, 1,
    1, 0, 1,
    1, 1, 1
    ), 3, byrow=TRUE)
  expect_error(gglayout(m), "matrix includes non-rectanglar submatrix.*")
})
