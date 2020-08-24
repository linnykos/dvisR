dvisR_system <- function(dat, cluster_labels = rep(NA, nrow(dat)),
                         feature_list = .grab_functions_in_package(),
                         system_options = system_options_default(), 
                         plotting_options = list(NA), 
                         debugging_inputs = list(NA)){
 
 # preprocessing, initialization stuff
 hash_pairs <- hash::hash(); .initialize_hash(hash_pairs)
 mat <- .initialize_feature_matrix(system_options$ntrials, system_options$new_pairs_per_round,
                                   system_options$minimum_instances_first_phase, names(feature_list))
 
 # phase 1 and phase 2
 phase_counter <- 1
 phase_completed <- rep(FALSE, 2)
 
 while(!all(phase_completed)){
  # sample new pairs :: need a check to ensure there are new pairs still
  
  # extract features of said pairs
  
  # find which to display :: depends on the phase
  
  # display and listen :: depends on the phase
 }
 
 
 # learn classifier
 classifier <- system_options$classifier(data = mat, label = vec)
  
 # prepare output
 .construct_dvisR(classifier = classifier)
}

####################

.construct_dvisR <- function(classifier){
 stopifnot(is.function(classifier))
 
 structure(list(classifier = classifier), class = "dvisR")
}