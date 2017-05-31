test_that("is_missing on data.table column", {
  d1 <- data.table(a = c(1,2,3), b = list(c("a", "b"), "c", c("d", "e")))
  d2 <- data.table(a = list(c(1,2,3), c(4, 5, 6), c(7, 8)), b = list(c("a", "b"), "c", c("d", "e")))
  d3 <- data.table(a = c(1,2,3), b = c(4,5,6), c  = c(7,8,9))
  
  out1 <- data.table(a = c(1,2,3), b = c(c("a,b"), "c", c("d,e")))
  out2 <- data.table(a = c("1,2,3", "4,5,6", "7,8"), b = c("a,b", "c", "d,e") )
  
  expect_equal(list2char(d1, sep = ","), out1)
  expect_equal(list2char(d2, sep = ","), out2)
  expect_equal(list2char(d3, sep = ","), d3)
  
  
  d1 <- data.table(a = c(1,2,3), b = c(4,NaN,6), c  = c(7,8,Inf))
  
  
  
  d1[, b2 := is_missing(b)]
  
  vec <- c("a", NaN, "b", NA, "d", Inf)
  
  x <- Inf
  is.null(x) | is.infinite(x) | is.na(x) | x == "" | x == " "
  
  sapply(vec, function(x) {is.null(x) | is.infinite(x) | is.na(x) | x == "" | x == " " })
  
  check <- is_missing(v, check_if_finite = T)
  
  
  sapply(vec, function(vec) {ifelse( is.na(vec) | is.null(vec) | (check_character & (vec == "" | vec == " ")) | (check_if_finite & is.infinite(vec)), TRUE, FALSE) })
  
  subset(v, check)
  
  v2 <- c("a", "b", "d")
  
  
})