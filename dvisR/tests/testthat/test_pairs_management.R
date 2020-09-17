context("Test pairs management")

## .generate_new_pairs is correct

test_that(".generate_new_pairs works", {
 set.seed(10)
 p <- 100
 new_pairs_per_round <- 10
 h <- .initialize_hash()
 
 res <- .generate_new_pairs(p, new_pairs_per_round, h)
 
 expect_true(is.matrix(res))
 expect_true(all(dim(res) == c(10, 2)))
 expect_true(all(!is.na(res)))
})

test_that(".generate_new_pairs affects h", {
 set.seed(10)
 p <- 100
 new_pairs_per_round <- 10
 h <- .initialize_hash()
 
 res <- .generate_new_pairs(p, new_pairs_per_round, h)
 
 expect_true(length(h) == 1 + new_pairs_per_round)
})

test_that(".generate_new_pairs affects h", {
 set.seed(10)
 p <- 100
 new_pairs_per_round <- 10
 h <- .initialize_hash()
 
 res <- .generate_new_pairs(p, new_pairs_per_round, h)
 
 expect_true(length(h) == 1 + new_pairs_per_round)
})

test_that(".generate_new_pairs generates h with unique values and unique keys", {
 set.seed(20)
 p <- 100
 new_pairs_per_round <- 10
 h <- .initialize_hash()
 
 # run it twice
 res <- .generate_new_pairs(p, new_pairs_per_round, h)
 res <- .generate_new_pairs(p, new_pairs_per_round, h)
 
 expect_true(length(unique(hash::keys(h))) == length(hash::keys(h)))
 tmp <- hash::values(h); tmp <- as.numeric(tmp[-which(names(tmp) == "count")])
 expect_true(length(unique(tmp)) == length(tmp))
})

test_that(".generate_new_pairs errors if you ask for too many pairs", {
 set.seed(20)
 p <- 5
 new_pairs_per_round <- 7
 h <- .initialize_hash()
 
 # run it twice
 res <- .generate_new_pairs(p, new_pairs_per_round, h)
 expect_error(.generate_new_pairs(p, new_pairs_per_round, h))
 
 # or run it once
 h <-  .initialize_hash()
 expect_error(.generate_new_pairs(p, 2*new_pairs_per_round, h))
})

#################

## .initialize_hash is correct

test_that(".initialize_hash works", {
 h <- .initialize_hash()
 
 expect_true(length(h) == 1)
 expect_true(hash::keys(h) == "count")
 expect_true(h[["count"]] == 0)
})

#################

