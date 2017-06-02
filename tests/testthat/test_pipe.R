test_that("`%l0>%` works with lists", {
  
  l <- list("a", 1, NA, 3, NULL, list() )
  
  expect_identical(l %l0>% 0, list("a", 1, NA, 3, 0, 0) )
})


test_that("`%l0>%` works with data.table", {
  d1 <- data.table(a = list(c(1,2,3), c(4, 5, 6), c(7, 8)), b = list(c("a", "b"), "c", list()), c = list(NULL, "f", NULL))
  
  d2 <- data.table(a = list(c(1,2,3), c(4, 5, 6), c(7, 8)), b = list(c("a", "b"), "c", 7  ), c = list(7, "f", 7))
  
  out <- copy(d1)
  out[, b := b %l0>% 7 ]
  out[, c := c %l0>% 7 ]
  
  expect_identical(out, d2)
})


