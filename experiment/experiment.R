rm(list=ls())
set.seed(10)
load("../experiment/test.RData")

cluster_labels = rep(NA, nrow(dat))
feature_list = .grab_functions_in_package()
system_options = system_options_default()
plotting_options = plotting_options_default()
plotting_module = plotting_module_base
debugging_inputs = NA
verbose = 1

.check_holistic(dat, plotting_options)

fl <- feature_list; so <- system_options; po <- plotting_options; pm <- plotting_module
di <- debugging_inputs
n <- nrow(dat); p <- ncol(dat)

# preprocessing, initialization stuff
hash_pairs <- .initialize_hash(); hash_na <- .initialize_na_handler()
feature_mat <- .initialize_feature_matrix(so$ntrials, so$new_pairs_per_round,
                                          so$minimum_instances_first_phase, names(fl))
pairs_mat <- matrix(NA, nrow = 0, ncol = 2)
response_vec <- rep(NA, nrow(feature_mat))
round_vec <- rep(NA, nrow(feature_mat))

round_idx <- 1
phase_counter <- 1
phase_completed <- rep(FALSE, 2)
number_requested <- prod(po$first_phase_gridsize)


# sample new pairs
counter <- hash_pairs[["count"]]
# check to see if we need to expand feature_mat and others
if(!is.na(feature_mat[counter+1,1])){
 feature_mat <- .expand_feature_matrix(feature_mat)
 response_vec <- .expand_response_vec(response_vec)
 round_vec <- .expand_response_vec(round_vec)
}
pairs_mat_new <- .generate_new_pairs(p, so$new_pairs_per_round[phase_counter], hash_pairs)

# extract features of new pairs
feature_mat_tmp <- .extract_features(dat, pairs_mat_new, fl)
if(any(is.na(feature_mat_tmp))){
 feature_mat_tmp <- .clean_na(feature_mat_tmp, hash_na, offset = counter)
}
feature_mat[(counter+1):(counter+nrow(feature_mat_tmp)),] <- feature_mat_tmp
hash_pairs[["count"]] <- counter+nrow(feature_mat_tmp)

pairs_mat <- rbind(pairs_mat, pairs_mat_new)

idx_vec <- so$learner_list$first_learner(feature_mat[1:hash_pairs[["count"]],], 
                                         response_vec[1:hash_pairs[["count"]]], 
                                         number_requested = number_requested,
                                         option_list = so$learner_options$first_learner)
idx_vec <- sort(idx_vec)
.check_indices(idx_vec, pairs_mat, hash_pairs)

## plot according to filter
pairs_submat <- pairs_mat[idx_vec,]
.plotter_first_phase(dat, pairs_submat, plotting_options = po, plotting_module = pm, i = round_idx, 
                     debugging_inputs = di)
