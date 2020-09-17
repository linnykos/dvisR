context("Test learner furtherest features")

## .rescale_and_separate is correct

test_that(".rescale_and_separate works", {
 set.seed(10)
 mat1 <- matrix(stats::rnorm(40), 4, 10)
 mat2 <- matrix(stats::rnorm(50), 5, 10)
 
 res <- .rescale_and_separate(mat1, mat2)
 
 expect_true(is.list(res))
 expect_true(length(res) == 2)
 expect_true(all(names(res) == c("mat1", "mat2")))
 expect_true(all(dim(res$mat1) == dim(mat1)))
 expect_true(all(dim(res$mat2) == dim(mat2)))
})

###################

## .distance_euclidean is correct

test_that(".rescale_and_separate works", {
 set.seed(20)
 mat1 <- matrix(stats::rnorm(40), 4, 10)
 mat2 <- matrix(stats::rnorm(50), 5, 10)
 
 res <- .distance_euclidean(mat1, mat2)
 
 expect_true(is.matrix(res))
 expect_true(all(dim(res) == c(nrow(mat1), nrow(mat2))))
 expect_true(is.numeric(res))
})

test_that(".rescale_and_separate gives the correct value", {
 set.seed(25)
 mat1 <- matrix(stats::rnorm(40), 4, 10)
 mat2 <- matrix(stats::rnorm(50), 5, 10)
 
 res <- .distance_euclidean(mat1, mat2)
 res2 <- matrix(NA, nrow(mat1), nrow(mat2))
 for(i in 1:nrow(mat1)){
  for(j in 1:nrow(mat2)){
   res2[i,j] <- .l2norm(mat1[i,] - mat2[j,])
  }
 }
 
 expect_true(sum(abs(res - res2)) <= 1e-6)
})

#####################

## learner_furtherest_distance is correct

test_that("learner_furtherest_distance works", {
 feature_mat <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)
 response_vec <- rep(c(NA,1), times = 10)
 number_requested <- 5
 option_list <- NA
 
 res <- learner_furtherest_distance(feature_mat, response_vec, number_requested, option_list)
 
 expect_true(length(res) == number_requested)
 expect_true(is.numeric(res))
 expect_true(all(res > 0))
 expect_true(all(res <= nrow(feature_mat)))
 expect_true(all(is.na(response_vec[res])))
})
