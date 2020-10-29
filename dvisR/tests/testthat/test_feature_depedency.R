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
  expect_true(names(res) == "Pearson")
})

test_that("feature_pearson works", {
  set.seed(10)
  dat <- matrix(stats::rnorm(100), 50, 2)
  cluster_label <- sample(c(1,2,3), 50, replace = T)
  res <- feature_pearson(use_cluster = T)(dat, cluster_label = cluster_label)
  
  expect_true(is.numeric(res))
  expect_true(length(res) == 2)
  expect_true(all(names(res) == c("Pearson_range", "Pearson_std")))
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
  expect_true(names(res) == "Kendall")
})

test_that("feature_kendall works", {
  set.seed(10)
  dat <- matrix(stats::rnorm(100), 50, 2)
  cluster_label <- sample(c(1,2,3), 50, replace = T)
  res <- feature_kendall(use_cluster = T)(dat, cluster_label = cluster_label)
  
  expect_true(is.numeric(res))
  expect_true(length(res) == 2)
  expect_true(all(names(res) == c("Kendall_range", "Kendall_std")))
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
  expect_true(names(res) == "Spearman")
})

test_that("feature_spearman works", {
  set.seed(10)
  dat <- matrix(stats::rnorm(100), 50, 2)
  cluster_label <- sample(c(1,2,3), 50, replace = T)
  res <- feature_spearman(use_cluster = T)(dat, cluster_label = cluster_label)
  
  expect_true(is.numeric(res))
  expect_true(length(res) == 2)
  expect_true(all(names(res) == c("Spearman_range", "Spearman_std")))
})
