test_that("unify_case_type works with data.table", {
  
  d1 <- data.table(a = c("A", "B", "C"), b = c("d", "e", "F")  )
  
  out1 <- unify_case_type(d1, cols2lower =   "a", cols2upper =  c() )
  expect_identical(out1, data.table(a = c("a", "b", "c"), b = c("d", "e", "F")  ))
  
  out2 <- unify_case_type(d1, cols2lower =   "a", cols2upper =  "b" )
  expect_identical(out2, data.table(a = c("a", "b", "c"), b = c("D", "E", "F")  ))
  
  expect_error(unify_case_type(d1, cols2lower =   "a", cols2upper =  "a" ))
  expect_error(unify_case_type(d1, cols2lower =   "a", cols2upper =  "B" ))
})