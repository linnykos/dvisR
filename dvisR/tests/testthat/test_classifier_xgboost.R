context("Test classifier XGBoost")

## classifier_xgboost_closure is correct

test_that("classifier_xgboost_closure works", {
 set.seed(10)
 
 data <- matrix(stats::rnorm(200), 200, 10)
 label <- rep(c(0,1), each = 100)
 
 res <- classifier_xgboost_closure()(data, label, verbose = 0)
 
 expect_true(class(res) == "xgb.Booster")
})
