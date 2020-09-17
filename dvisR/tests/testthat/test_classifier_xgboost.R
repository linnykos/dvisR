context("Test classifier XGBoost")

## classifier_xgboost_closure is correct

test_that("classifier_xgboost_closure works", {
 set.seed(10)
 
 data <- matrix(stats::rnorm(200), 200, 10)
 label <- rep(c(0,1), each = 100)
 
 res <- classifier_xgboost_closure()(data, label, verbose = 0)
 
 expect_true(class(res) == "xgb.Booster")
})

###############################

## predict_xgboost_wrapper is correct

test_that("predict_xgboost_wrapper works", {
 set.seed(10)
 
 data <- matrix(stats::rnorm(200), 200, 10)
 label <- rep(c(0,1), each = 100)
 
 classifier <- classifier_xgboost_closure()(data, label, verbose = 0)
 newdata <- matrix(stats::rnorm(40), 4, 10)
 
 res <- predict_xgboost_wrapper(classifier, newdata)
 
 expect_true(is.numeric(res))
 expect_true(length(res) == 1)
 expect_true(all(res >= 0))
 expect_true(all(res <= 1))
})
