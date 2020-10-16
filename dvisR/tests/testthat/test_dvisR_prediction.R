context("Test dvisR_prediction")

## .compute_remaining_feature_mat is correct

test_that(".compute_remaining_feature_mat works", {
 set.seed(10)
 n <- 200; p <- 50
 dat <- MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = diag(p))
 obj <- dvisR_system(dat, system_options = system_options_default(ntrials = 10, minimum_instances_first_phase = 4),
                     debugging_inputs = list(round_inputs = list("1,2,3", "1,5,6", 
                                                                 "y", "y", "y", "n", "y", "y", "n", "n", "n", "y")))
 
 tmp <- .extract_mat_response(obj)
 res <- .compute_remaining_feature_mat(obj$df[,c("Idx1", "Idx2")], dat, feature_list = obj$feature_list)
 
 expect_true(is.list(res))
 expect_true(length(res) == 2)
 expect_true(all(sort(names(res)) == sort(c("pairs_mat", "feature_mat"))))
 expect_true(is.matrix(res$pairs_mat))
 expect_true(is.matrix(res$feature_mat))
 expect_true(ncol(res$pairs_mat) == 2)
 expect_true(ncol(res$feature_mat) == length(obj$feature_list))
 expect_true(nrow(res$pairs_mat) == p*(p-1)/2 - nrow(obj$df))
})

##########################################3

## dvisR_prediction is corrrect

test_that("dvisR_prediction works", {
 set.seed(10)
 n <- 200; p <- 50
 dat <- MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = diag(p))
 obj <- dvisR_system(dat, system_options = system_options_default(ntrials = 10, minimum_instances_first_phase = 4),
                     debugging_inputs = list(round_inputs = list("1,2,3", "1,5,6", 
                                                                 "y", "y", "y", "n", "y", "y", "n", "n", "n", "y")))
 
 res <- dvisR_prediction(obj, dat)
 
 expect_true(is.list(res))
 expect_true(class(res) == "dvisR_prediction")
 expect_true(all(sort(names(res)) == sort(c("df", "probability", "prediction"))))
 expect_true(all(dim(res$df) == c(p*(p-1)/2, ncol(obj$df))))
 expect_true(all(colnames(res$df) == colnames(obj$df)))
 expect_true(all(res$probability >= 0))
 expect_true(all(res$probability <= 1))
 expect_true(all(res$prediction %in% c(0, 1)))
})

test_that("dvisR_prediction correctly labels 'y' as dependency", {
 set.seed(10)
 n <- 1000; p <- 20
 sig_mat <- matrix(0, p, p); sig_mat[1:10,1:10] <- 0.9; sig_mat[11:20,11:20] <- 0.9; diag(sig_mat) <- 1
 
 dat <- MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = sig_mat)
 
 set.seed(10)
 obj <- dvisR_system(dat, system_options = system_options_default(ntrials = 10, minimum_instances_first_phase = 4),
                     debugging_inputs = list(round_inputs = list("4,9", "1,2,3,4,5,6,7,8,9", 
                                                                 "n","n","n","n","n",  "y","n","y","n","y")))
 
 res <- dvisR_prediction(obj, dat)
 
 expected_res <- sapply(1:nrow(res$df), function(x){
  idx1 <- res$df[x,"Idx1"]; idx2 <- res$df[x,"Idx2"]
  ifelse(sig_mat[idx1,idx2] > 0.1, 1, 0) 
 })
 
 expect_true(all(res$prediction == expected_res))
})

