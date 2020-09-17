context("Test system backend")

## .check_system_options is correct

test_that(".check_system_options works", {
 classifier <- classifier_xgboost_closure()
 ntrials <- 5
 learner_list <- list(first_learner = learner_furtherest_distance, 
                     second_learner = learner_furtherest_distance)
 learner_options <- list(first_learner = NA, 
                        second_learner = NA)
 new_pairs_per_round <- c(20,10)
 minimum_instances_first_phase <- 10
 
 res <- .check_system_options(classifier, ntrials, learner_list, learner_options, new_pairs_per_round, 
                              minimum_instances_first_phase)
 
 expect_true(length(res) == 0)
})

###############

## system_options_default is correct

test_that("system_options_default works", {
 res <- system_options_default()
 
 expect_true(is.list(res))
})
