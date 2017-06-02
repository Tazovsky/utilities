
test_that("source dir works", {
  dir <- "test_function_source_dir"
    
  dir.create(dir, showWarnings = F)
  output <- 'Hello World! :)'
  cat(sprintf("print('%s')", output), file = paste0(dir, '/helloworld.R'))
  
  expect_output(source_dir(dir), output)
  
  unlink(dir, T, T) # usuwam katalog
})