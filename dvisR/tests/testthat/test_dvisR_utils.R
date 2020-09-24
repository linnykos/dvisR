context("Test dvisR utils")

## .check_holistic is correct

test_that(".check_holistic works", {
 dat <- matrix(1:30,6,5)
 feature_list <- .grab_functions_in_package()
 plotting_options <- plotting_options_default()
 
 res <- .check_holistic(dat, feature_list, plotting_options)
 
 expect_true(length(res) == 0)
})

test_that(".check_holistic works with colors", {
 dat <- matrix(1:30,6,5)
 feature_list <- .grab_functions_in_package()
 plotting_options <- plotting_options_default(color_vec = c(1:6))
 
 res <- .check_holistic(dat,  feature_list, plotting_options)
 
 expect_true(length(res) == 0)
})

test_that(".check_holistic works can error", {
 dat <- matrix(1:30,6,5)
 feature_list <- .grab_functions_in_package()
 plotting_options <- plotting_options_default(color_vec = c(1:5))
 
 expect_error(.check_holistic(dat, feature_list, plotting_options))
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