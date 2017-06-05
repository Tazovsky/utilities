context("remove_whitespaces")

test_that("remove_whitespaces on data.table column", {
  dt1 <- data.table(a = c("1,2,3", "4,5,6", "7,8"), b = c("a,b", "c", "d,e") )
  
  out1 <- data.table(a = c(" 1, 2, 3 ", "4, 5, 6", "7, 8"), b = c(" a, b ", paste0(intToUtf8(160),"c", intToUtf8(160)
), "d ,         e") )
  
  out1[, a := remove_whitespaces(a,  beginning = FALSE, end = FALSE )]
  out1[, b := remove_whitespaces(b,  beginning = FALSE, end = FALSE )]
  
  
  expect_equal(out1, dt1)
  
})

test_that("remove_whitespaces on string with a lot of whitespaces", {
  s1 <- "

X	Y     W"
  
  out1 <- remove_whitespaces(s1,  beginning = FALSE, end = FALSE )
  expect_equal(unname(out1), "XYW")
})


test_that("check if string is a string", {
  expect_error(remove_whitespaces(list(1,2,"s")), "not a string")
})

test_that("check if string is not empty", {
  expect_error(remove_whitespaces(""), "is empty")
})

test_that("beginnig anr/or and are logical", {
  expect_error(remove_whitespaces(" s s s", beginning = "TRUE", end = T), "not a logical")
})



