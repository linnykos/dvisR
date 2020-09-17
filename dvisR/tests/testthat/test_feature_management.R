context("Test feature management")

## .grab_functions_in_package is correct

test_that(".grab_functions_in_package works", {
 res <- .grab_functions_in_package()
 
 expect_true(is.list(res))
 expect_true(all(sapply(res, is.function)))
})

test_that(".grab_functions_in_package yields functions that work", {
 set.seed(10)
 res <- .grab_functions_in_package()
 dat <- matrix(rnorm(1:20), 10, 2)
 
 val <- sapply(res, function(func){func(dat)})
 
 expect_true(all(is.numeric(val)))
 expect_true(length(res) == length(val))
})

########################

## .apply_feature_list is correct

test_that(".apply_feature_list works", {
 set.seed(10)
 feature_list <- .grab_functions_in_package()
 dat_2col <- matrix(rnorm(1:20), 10, 2)
 
 res <- .apply_feature_list(dat_2col, feature_list)
 
 expect_true(all(names(res) == names(feature_list)))
 expect_true(all(is.numeric(res)))
})

test_that(".apply_feature_list has meaningful tryCatch", {
 set.seed(10)
 dat_2col <- matrix(rnorm(1:20), 10, 2)
 feature_list <- .grab_functions_in_package()
 len <- length(feature_list)
 feature_list[[len+1]] <- function(x){stop()}
 names(feature_list)[len+1] <- "Test"
 
 expect_error(feature_list[[len+1]](dat_2col))
 
 res <- .apply_feature_list(dat_2col, feature_list)
 
 expect_true(all(names(res) == names(feature_list)))
 expect_true(is.na(res[len+1]))
 expect_true(names(res)[len+1] == "Test")
})


########################

## .extract_features is correct

test_that(".extract_features works", {
 set.seed(10)
 feature_list <- .grab_functions_in_package()
 dat <- matrix(rnorm(1:50), 10, 5)
 pairs_mat <- matrix(c(1,2, 3,4, 1,5), nrow = 3, ncol = 2, byrow = T)
 
 res <- .extract_features(dat, pairs_mat, feature_list)
 
 expect_true(all(dim(res) == c(nrow(pairs_mat), length(feature_list))))
 expect_true(all(is.numeric(res)))
})
