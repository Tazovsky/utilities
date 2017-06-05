context("is_missing")

test_that("is_missing: subset not missing values from vector", {
  vec <- c(1, NaN, 2, NA, 3, Inf)
  
  out <- is_missing(vec, check_if_finite = T)
  
  expect_identical(vec[!out], c(1, 2, 3))
})


test_that("is_missing: checking missing values in data.table columns", {
  d1 <- data.table(a = c(1,2,NA), b = c(NaN,5,6), c  = c(Inf,8,9))
  
  cols2check <- c("a", "b", "c")
  
  d1[, (cols2check) := lapply(.SD, function(x) ifelse(is_missing(x, check_if_finite = T), 0, x)), .SDcols = cols2check]
  
  d2 <- data.table(a = c(1,2,0), b = c(0,5,6), c  = c(0,8,9))
  
  expect_identical(d1, d2) 
})

test_that("check_if_finite is logical", {
  expect_error(is_missing(c(1, NaN, 2, NA, 3, Inf), check_if_finite = "TRUE"), "not a logical value")
})

test_that("vec is not empty", {
  expect_error(is_missing(c(), check_if_finite = TRUE), "is empty")
})


test_that("vec is not a vactor", {
  expect_error( is_missing(list(1,NaN,3)), "not a vector")
})


