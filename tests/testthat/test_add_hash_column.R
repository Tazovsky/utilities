context("add_hash_column")

test_that("add_hash_column calculating method has changed", {
  
  data <- data.table(a = c("Ala", "Kot", "q"), b = c("Ma", "Ma", "wer"), c = c("Kota", "Ale", "ty"))
  
  dt_hash <- add_hash_column(data, colnames_for_hash = c("a", "b"), hash_colname = "hash",
                             excluded_colnames = c(), unite = TRUE, cores = 1L,
                             sort_colnames_for_hash = TRUE)
  
  # wyliczone wczesniej; sposob nie moze sie zmienic poniewaz przy zalozeniu ze hash bedzie uzyty na bazie jako np Primary Key musi zachowac swoja unikalnosc
  hashes <- data.table(hash = c("561d77f71c14a31f9252b93909ff9e3d", "eefab55816293d244e06114c79055538", "b2fa7a5beff6319ff5f2bf6e8a476056"))
  
  expect_equal(dt_hash[, hash], hashes[, hash] )
})

test_that("add_hash_column calculating method has changed (UNITE = FALSE)", {
  
  data <- data.table(a = c("Ala", "Kot", "q"), b = c("Ma", "Ma", "wer"), c = c("Kota", "Ale", "ty"))
  
  dt_hash_w_o_unite <- add_hash_column(data, colnames_for_hash = c("a", "b"), hash_colname = "hash",
                                       excluded_colnames = c(), unite = FALSE, cores = 1L,
                                       sort_colnames_for_hash = TRUE)
  
  hashes <-  data.table(hash = c("84684a15b1b2df1607c9bc4c1672cd8f", "b71f7842be116858ba5dd0c23b3c1890", "862c10c9a368064ae28cd5694838939f"))
  
  expect_equal(dt_hash_w_o_unite[, hash], hashes[, hash] )
  
})

test_that("Verification of function arguments classes", {
  data <- data.table(a = c("Ala", "Kot", "q"), b = c("Ma", "Ma", "wer"), c = c("Kota", "Ale", "ty"))
  
  expect_error(add_hash_column(data, colnames_for_hash = c("a", "b"), hash_colname = 1,
                               excluded_colnames = c(), unite = FALSE, cores = 1L,
                               sort_colnames_for_hash = TRUE), "not a string" )
  
  expect_error(add_hash_column(data, colnames_for_hash = c("a", "b"), hash_colname = "hash",
                               excluded_colnames = c(), unite = "FALSE", cores = 1L,
                               sort_colnames_for_hash = TRUE), "not a logical" )
  
  expect_error(add_hash_column(data, colnames_for_hash = c("a", "b"), hash_colname = "hash",
                               excluded_colnames = c(), unite = FALSE, cores = 1L,
                               sort_colnames_for_hash = "TRUE"), "not a logical" )
  
  expect_error(add_hash_column(data, colnames_for_hash = c("a", "b"), hash_colname = "hash",
                               excluded_colnames = c(), unite = FALSE, cores = 1.0,
                               sort_colnames_for_hash = T), "not an integer" )
  
  cores <- as.integer(parallel::detectCores() + 2)
  expect_error(add_hash_column(data, colnames_for_hash = c("a", "b"), hash_colname = "hash",
                               excluded_colnames = c(), unite = FALSE, cores = cores,
                               sort_colnames_for_hash = T), "number of cores is unavailable" )
  
  expect_error(add_hash_column(data, colnames_for_hash = list("a", "b"), hash_colname = "hash",
                               excluded_colnames = c(), unite = FALSE, cores = 1L,
                               sort_colnames_for_hash = T), "not a vector" )
  
  expect_error(add_hash_column(data, colnames_for_hash = c("a", "b"), hash_colname = "hash",
                               excluded_colnames = list("b"), unite = FALSE, cores = 1L,
                               sort_colnames_for_hash = T), "not a vector" )
  
  expect_error(add_hash_column(data, colnames_for_hash = c(), hash_colname = "hash",
                               excluded_colnames = list("b"), unite = FALSE, cores = 1L,
                               sort_colnames_for_hash = T), "not a vector" )
  
  expect_error(add_hash_column(mtcars, colnames_for_hash = c("a", "b"), hash_colname = "hash",
                               excluded_colnames = c(), unite = FALSE, cores = 1L,
                               sort_colnames_for_hash = T), "not a data\\.table" )
  
  expect_error(add_hash_column(data, colnames_for_hash = c("a", "d", "e"), hash_colname = "hash",
                               excluded_colnames = c(), unite = FALSE, cores = 1L,
                               sort_colnames_for_hash = T), "Column.*missing" )
  
  
    
})


