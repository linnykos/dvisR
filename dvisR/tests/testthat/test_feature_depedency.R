context("Test feature dependency")

## feature_pearson is correct

test_that("feature_pearson works", {
 set.seed(10)
 dat <- matrix(stats::rnorm(20), 10, 2)
 res <- feature_pearson()(dat)
 
 expect_true(is.numeric(res))
 expect_true(length(res) == 1)
 expect_true(res <= 1)
 expect_true(res >= -1)
})

######################

## feature_kendall is correct

test_that("feature_kendall works", {
 set.seed(10)
 dat <- matrix(stats::rnorm(20), 10, 2)
 res <- feature_kendall()(dat)
 
 expect_true(is.numeric(res))
 expect_true(length(res) == 1)
 expect_true(res <= 1)
 expect_true(res >= -1)
})

######################

## feature_spearman is correct

test_that("feature_spearman works", {
 set.seed(10)
 dat <- matrix(stats::rnorm(20), 10, 2)
 res <- feature_spearman()(dat)
 
 expect_true(is.numeric(res))
 expect_true(length(res) == 1)
 expect_true(res <= 1)
 expect_true(res >= -1)
})