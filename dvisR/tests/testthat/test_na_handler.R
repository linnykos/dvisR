context("Test NA handler")

## .initialize_na_handler is correct

test_that(".initialize_na_handler works", {
 h <- .initialize_na_handler()
 
 expect_true(class(h) == "hash")
 expect_true(length(h) == 0)
})

######################

## .clean_na is correct

test_that(".clean_na works", {
 set.seed(10)
 feature_mat <- matrix(1:200, 20, 10)
 idx <- sample(1:200, 10)
 feature_mat[idx] <- NA
 
 h <- .initialize_na_handler()
 
 res <- .clean_na(feature_mat, h)
 
 expect_true(all(!is.na(res[idx])))
 expect_true(length(h) == length(idx))
 
 keys <- hash::keys(h)
 bool_vec <- sapply(keys, function(key){
  idx <- h[[key]]
  is.na(feature_mat[idx[1], idx[2]])
 })
 
 expect_true(all(bool_vec))
})

#########################

## .restore_na is correct

test_that(".restore_na works", {
 set.seed(10)
 feature_mat <- matrix(1:200, 20, 10)
 idx <- sample(1:200, 10)
 feature_mat[idx] <- NA
 
 h <- .initialize_na_handler()
 feature_mat2 <- .clean_na(feature_mat, h)
 
 res <- .restore_na(feature_mat2, h)
 
 expect_true(all.equal(feature_mat, res))
 expect_true(sum(is.na(feature_mat)) == sum(is.na(res)))
})
