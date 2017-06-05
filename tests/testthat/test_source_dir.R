context("source_dir")

test_that("source dir works", {
  dir <- "test_function_source_dir"
    
  dir.create(dir, showWarnings = F)
  output <- 'Hello World! :)'
  cat(sprintf("print('%s')", output), file = paste0(dir, '/helloworld.R'))
  
  expect_output(source_dir(dir), output)
  
  unlink(dir, T, T) # usuwam katalog
})

test_that("path does not exists", {
  expect_error(source_dir(paste0("test_dir_" , as.integer(Sys.time()))), "not exist")
})

test_that("path does not exists", {
  expect_error(source_dir(paste0("test_dir_" , as.integer(Sys.time()))), "not exist")
})

test_that("path is character", {
  expect_error(source_dir(1), "not a character")
})



