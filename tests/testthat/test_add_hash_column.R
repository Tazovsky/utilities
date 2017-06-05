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
