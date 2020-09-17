context("Test dvisR")

## .check_holistic is correct

test_that(".check_holistic works", {
 dat <- matrix(1:30,6,5)
 plotting_options <- plotting_options_default()
 
 res <- .check_holistic(dat, plotting_options)
 
 expect_true(length(res) == 0)
})

test_that(".check_holistic works with colors", {
 dat <- matrix(1:30,6,5)
 plotting_options <- plotting_options_default(color_vec = c(1:6))
 
 res <- .check_holistic(dat, plotting_options)
 
 expect_true(length(res) == 0)
})

test_that(".check_holistic works can error", {
 dat <- matrix(1:30,6,5)
 plotting_options <- plotting_options_default(color_vec = c(1:5))
 
 expect_error(.check_holistic(dat, plotting_options))
})

##########################

## .string_parser is correct

test_that(".string_parser works", {
 res <- .string_parser("2")
 
 expect_true(res == 2)
 expect_true(length(res) == 1)
 expect_true(is.numeric(res))
})

test_that(".string_parser works for a string of values", {
 res <- .string_parser("2 1 45 2")
 
 expect_true(all(res == c(2, 1, 45, 2)))
 expect_true(length(res) == 4)
 expect_true(is.numeric(res))
})

test_that(".string_parser works for a string of values with commas and spaces", {
 res <- .string_parser("2, 1, 45, 2")
 
 expect_true(all(res == c(2, 1, 45, 2)))
 expect_true(length(res) == 4)
 expect_true(is.numeric(res))
})

test_that(".string_parser works for a string of values with commas", {
 res <- .string_parser("2,1,45,2")
 
 expect_true(all(res == c(2, 1, 45, 2)))
 expect_true(length(res) == 4)
 expect_true(is.numeric(res))
})

test_that(".string_parser removes letters", {
 res <- .string_parser("2,1,45a,2,asdf")
 
 expect_true(all(res == c(2, 1, 45, 2)))
 expect_true(length(res) == 4)
 expect_true(is.numeric(res))
})

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




