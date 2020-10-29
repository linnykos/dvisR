context("Test dvisR")


######################

## .response_handler_first_phase is correct

test_that(".response_handler_first_phase works", {
 vec <- c(1,4,6,2)
 number_requested <- 9
 user_input <- paste0(vec, collapse = ",")
 res <- .response_handler_first_phase(user_input, number_requested)
 
 res2 <- rep(0, number_requested)
 res2[vec] <- 1
 
 expect_true(all(res == res2))
})

#########################

## .response_handler_second_phase is corrrect

test_that(".response_handler_second_phase works", {
 user_input <- "y"
 res <- .response_handler_second_phase(user_input)
 
 expect_true(length(res) == 1)
 expect_true(is.numeric(res))
 expect_true(res == 1)
 
 user_input <- "n"
 res <- .response_handler_second_phase(user_input)
 
 expect_true(length(res) == 1)
 expect_true(is.numeric(res))
 expect_true(res == 0)
})

############################

## .response_listener_first_phase is correct

test_that(".response_listener_first_phase works", {
 i <- 2
 number_requested <- 9
 debugging_inputs <- list(round_inputs = list(c("1,4,6,2"), c("1,3,2"), c("5")))
 
 res <- .response_listener_first_phase(i, number_requested, debugging_inputs)
 
 expect_true(is.numeric(res))
 expect_true(length(res) == number_requested)
 expect_true(all(res == c(1,1,1,0,0,0,0,0,0)))
})

test_that(".response_listener_first_phase can handle no input", {
  i <- 2
  number_requested <- 9
  debugging_inputs <- list(round_inputs = list(c("1,4,6,2"), c(""), c("5")))
  
  res <- .response_listener_first_phase(i, number_requested, debugging_inputs)
  
  expect_true(is.numeric(res))
  expect_true(length(res) == number_requested)
  expect_true(all(res == rep(0,number_requested)))
})

test_that(".response_listener_first_phase over all 3 rounds", {
 i <- 2
 number_requested <- 9
 round_list <- list(c(1,4,6,2), c(1,3,2), 5)
 debugging_inputs <- list(round_inputs = lapply(round_list, function(x){paste0(x, collapse = ",")}))
 
 for(i in 1:3){
  res <- .response_listener_first_phase(i, number_requested, debugging_inputs)
  
  res2 <- rep(0, number_requested)
  res2[round_list[[i]]] <- 1
  
  expect_true(is.numeric(res))
  expect_true(length(res) == number_requested)
  expect_true(all(res == res2))
 }
})

################################

test_that(".response_listener_second_phase works", {
 i <- 2
 debugging_inputs <- list(round_inputs = list("y", "n", "y"))
 res <- .response_listener_second_phase(i, debugging_inputs)
 
 expect_true(is.numeric(res))
 expect_true(length(res) == 1)
 expect_true(res == 0)
})

test_that(".response_listener_second_phase can handle no input", {
  i <- 2
  debugging_inputs <- list(round_inputs = list("y", "", "y"))
  res <- .response_listener_second_phase(i, debugging_inputs)
  
  expect_true(is.numeric(res))
  expect_true(length(res) == 1)
  expect_true(res == 0)
})

test_that(".response_listener_second_phase works over all 3 rounds", {
 debugging_inputs <- list(round_inputs = list("y", "n", "y"))
 res2 <- c(1,0,1)
 
 for(i in 1:3){
  res <- .response_listener_second_phase(i, debugging_inputs)
  expect_true(is.numeric(res))
  expect_true(length(res) == 1)
  expect_true(res == res2[i])
 }
})

############################################

## dvisR_system is correct

test_that("dvisR_system works", {
  set.seed(10)
  dat <- MASS::mvrnorm(n = 200, mu = rep(0, 50), Sigma = diag(50))
  res <- dvisR_system(dat, system_options = system_options_default(ntrials = 10, minimum_instances_first_phase = 4),
                      debugging_inputs = list(round_inputs = list("1,2,3", "1,5,6", 
                                                                  "y", "y", "y", "n", "y", "y", "n", "n", "n", "y")))
  
  expect_true(is.list(res))
  expect_true(all(sort(names(res)) == sort(c("df", "dim", "cluster_labels", "fit_classifier", "feature_list", "system_options"))))
})



