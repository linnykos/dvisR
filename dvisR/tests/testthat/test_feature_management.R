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

#########################

## .initialize_feature_matrix is correct

test_that(".initialize_feature_matrix works", {
 feature_list <- .grab_functions_in_package()
 feature_names <- names(feature_list)
 
 ntrials <- 6
 new_pairs_per_round <- c(5,5)
 minimum_instances_first_phase <- 10
 
 res <- .initialize_feature_matrix(ntrials, new_pairs_per_round, minimum_instances_first_phase, feature_names)
 
 expect_true(all(is.na(res)))
 expect_true(all(colnames(res) == feature_names))
 expect_true(nrow(res) == minimum_instances_first_phase*new_pairs_per_round[1] + ntrials*new_pairs_per_round[2])
})

#########################

## .initial_response_vec is correct

test_that(".initial_response_vec works", {
 ntrials <- 6
 new_pairs_per_round <- c(5,5)
 minimum_instances_first_phase <- 10
 
 res <- .initial_response_vec(ntrials, new_pairs_per_round, minimum_instances_first_phase)
 
 expect_true(all(is.na(res)))
 expect_true(length(res) == minimum_instances_first_phase*new_pairs_per_round[1] + ntrials*new_pairs_per_round[2])
})

#########################

## .expand_feature_matrix is correct

test_that(".expand_feature_matrix works", {
 feature_list <- .grab_functions_in_package()
 feature_names <- names(feature_list)
 
 ntrials <- 6
 new_pairs_per_round <- c(5,5)
 minimum_instances_first_phase <- 10
 
 feature_mat <- .initialize_feature_matrix(ntrials, new_pairs_per_round, minimum_instances_first_phase, feature_names)
 feature_mat[1:nrow(feature_mat), 1:ncol(feature_mat)] <- sample(1:100, prod(dim(feature_mat)), replace = T)
 
 res <- .expand_feature_matrix(feature_mat, scaling = 1.2)
 
 expect_true(all(is.na(res[(nrow(feature_mat)+1):nrow(res),])))
 expect_true(nrow(res) >= 1.19*nrow(feature_mat))
 expect_true(sum(abs(as.matrix(feature_mat) - as.matrix(res[1:nrow(feature_mat),]))) <= 1e-6)
})

#########################

## .expand_response_vec is correct

test_that(".expand_response_vec works", {
 feature_list <- .grab_functions_in_package()
 feature_names <- names(feature_list)
 
 ntrials <- 6
 new_pairs_per_round <- c(5,5)
 minimum_instances_first_phase <- 10
 
 feature_mat <- .initialize_feature_matrix(ntrials, new_pairs_per_round, minimum_instances_first_phase, feature_names)
 feature_mat[1:nrow(feature_mat), 1:ncol(feature_mat)] <- sample(1:100, prod(dim(feature_mat)), replace = T)
 feature_mat <- .expand_feature_matrix(feature_mat, scaling = 1.2)
 
 response_vec <- .initial_response_vec(ntrials, new_pairs_per_round, minimum_instances_first_phase)
 response_vec[1:length(response_vec)] <- sample(1:10, length(response_vec), replace = T)
 res <- .expand_response_vec(response_vec, feature_mat)
 
 expect_true(length(res) == nrow(feature_mat))
 expect_true(sum(abs(response_vec - res[1:length(response_vec)])) <= 1e-6)
 expect_true(all(is.na(res[(length(response_vec)+1):length(res)])))
})

##############################

## .clean_feature_matrix is correct

test_that(".clean_feature_matrix works", {
 feature_list <- .grab_functions_in_package()
 feature_names <- names(feature_list)
 
 ntrials <- 6
 new_pairs_per_round <- c(5,5)
 minimum_instances_first_phase <- 10
 
 feature_mat <- .initialize_feature_matrix(ntrials, new_pairs_per_round, minimum_instances_first_phase, feature_names)
 feature_mat[1:nrow(feature_mat), 1:ncol(feature_mat)] <- sample(1:100, prod(dim(feature_mat)), replace = T)
 feature_mat2 <- .expand_feature_matrix(feature_mat, scaling = 1.2)
 
 res <- .clean_feature_matrix(feature_mat2)
 
 expect_true(all(dim(res) == dim(feature_mat)))
 expect_true(sum(abs(as.matrix(res) - as.matrix(feature_mat))) <= 1e-6)
})

##############################

## .clean_response_vec is correct

test_that(".clean_response_vec works", {
 feature_list <- .grab_functions_in_package()
 feature_names <- names(feature_list)
 
 ntrials <- 6
 new_pairs_per_round <- c(5,5)
 minimum_instances_first_phase <- 10
 
 feature_mat <- .initialize_feature_matrix(ntrials, new_pairs_per_round, minimum_instances_first_phase, feature_names)
 feature_mat[1:nrow(feature_mat), 1:ncol(feature_mat)] <- sample(1:100, prod(dim(feature_mat)), replace = T)
 feature_mat <- .expand_feature_matrix(feature_mat, scaling = 1.2)
 
 response_vec <- .initial_response_vec(ntrials, new_pairs_per_round, minimum_instances_first_phase)
 response_vec[1:length(response_vec)] <- sample(1:10, length(response_vec), replace = T)
 response_vec2 <- .expand_response_vec(response_vec, feature_mat)
 
 feature_mat <- .clean_feature_matrix(feature_mat)
 res <- .clean_response_vec(response_vec2, feature_mat)
 
 expect_true(all(length(res) == nrow(feature_mat)))
 expect_true(all(length(res) == length(response_vec)))
 expect_true(sum(abs(res - response_vec)) <= 1e-6)
})


