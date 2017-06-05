context("unify_case_type")

test_that("unify_case_type works with data.table", {
  
  d1 <- data.table(a = c("A", "B", "C"), b = c("d", "e", "F")  )
  
  out1 <- unify_case_type(d1, cols2lower =   "a", cols2upper =  c() )
  expect_identical(out1, data.table(a = c("a", "b", "c"), b = c("d", "e", "F")  ))
  
  out2 <- unify_case_type(d1, cols2lower =   "a", cols2upper =  "b" )
  expect_identical(out2, data.table(a = c("a", "b", "c"), b = c("D", "E", "F")  ))
  
  expect_error(unify_case_type(d1, cols2lower =   "a", cols2upper =  "a" ))
  expect_error(unify_case_type(d1, cols2lower =   "a", cols2upper =  "B" ))
})

test_that("unify_case_type arguments verification", {
  dt <- data.table(a = c("A", "B", "C"), b = c("d", "e", "F")  )
  
  expect_error(unify_case_type(mtcars, cols2lower = "mpg", cols2upper =  c() ), "is\\.data\\.table.*TRUE" )
  expect_error(unify_case_type(data.table(), cols2lower = "mpg", cols2upper =  c() ), "not greater than 0" )
  expect_error(unify_case_type(dt, cols2lower = list("a"), cols2upper =  c("b") ), "is not a vector" )
  expect_error(unify_case_type(dt, cols2lower = c("a"), cols2upper =  list("b") ), "is not a vector" )
  expect_error(unify_case_type(dt, cols2lower = c("a"), cols2upper =  c("b", "a") ), "convert same columns" )
  expect_error(unify_case_type(dt, cols2lower = c(), cols2upper =  c() ), "Both.*are empty" )
  expect_error(unify_case_type(dt, cols2lower = c("q"), cols2upper =  c("a") ), "Column.*are missing" )
  expect_error(unify_case_type(dt, cols2lower = c("a"), cols2upper =  c("w") ), "Column.*are missing" )
  
  dt2 <- data.table(a = list("A", "B", list("C", "D") ), b = c("d", "e", "F")  )
  expect_error(unify_case_type(dt2, cols2lower = c("a"), cols2upper =  c("b") ), "not support lists")
  
  
})