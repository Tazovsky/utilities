context("list2char")

test_that("list2char works with of list columns in data.table", {
  d1 <- data.table(a = c(1,2,3), b = list(c("a", "b"), "c", c("d", "e")))
  d2 <- data.table(a = list(c(1,2,3), c(4, 5, 6), c(7, 8)), b = list(c("a", "b"), "c", c("d", "e")))
  d3 <- data.table(a = c(1,2,3), b = c(4,5,6), c  = c(7,8,9))
  
  out1 <- data.table(a = c(1,2,3), b = c(c("a,b"), "c", c("d,e")))
  out2 <- data.table(a = c("1,2,3", "4,5,6", "7,8"), b = c("a,b", "c", "d,e") )
  
  expect_equal(list2char(d1, sep = ","), out1)
  expect_equal(list2char(d2, sep = ","), out2)
  expect_equal(list2char(d3, sep = ","), d3)
  
})

test_that("list2char works with data.table without any list columns", {
  expect_warning(list2char(data.table(mtcars)), "None of columns is 'list' class")
})

test_that("'data' argument is data.table", {
  expect_error(list2char(mtcars), "is\\.data\\.table.*TRUE")
})

test_that("'expect' is vector", {
  d2 <- data.table(a = list(c(1,2,3), c(4, 5, 6), c(7, 8)), b = list(c("a", "b"), "c", c("d", "e")))
  expect_error(list2char(d2, except = list("a", "b") ), "not a character vector")
})

test_that("'expect' cointains correct column names", {
  d2 <- data.table(a = list(c(1,2,3), c(4, 5, 6), c(7, 8)), b = list(c("a", "b"), "c", c("d", "e")))
  expect_error(list2char(d2, except = c("d", "b") ), "no columns")
})

test_that("separator has to be a character class", {
  d2 <- data.table(a = list(c(1,2,3), c(4, 5, 6), c(7, 8)), b = list(c("a", "b"), "c", c("d", "e")))
  expect_error(list2char(d2, sep = 1 ), "not a character")
})


