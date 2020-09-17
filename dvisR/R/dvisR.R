dvisR_system <- function(dat, cluster_labels = rep(NA, nrow(dat)),
                         feature_list = .grab_functions_in_package(),
                         system_options = system_options_default(), 
                         plotting_options = list(NA),  ## CREATE SEPERATE PLOTTING MODULE AS WELL
                         debugging_inputs = list(NA), verbose = 1, ...){
  
  if(!is.all(is.na(cluster_labels))) stopifnot(nrow(dat) == length(cluster_labels))
  
  fl <- feature_list; so <- system_options; po <- plotting_options; di <- debugging_inputs
  p <- ncol(dat)
  
  # preprocessing, initialization stuff
  hash_pairs <- .initialize_hash(); hash_na <- .initialize_na_handler()
  feature_mat <- .initialize_feature_matrix(so$ntrials, so$new_pairs_per_round,
                                    so$minimum_instances_first_phase, names(fl))
  pairs_mat <- matrix(NA, nrow = 0, ncol = 2)
  response_vec <- rep(NA, nrow(feature_mat))
  
  # phase 1 and phase 2
  phase_counter <- 1
  phase_completed <- rep(FALSE, 2)
  
  while(!all(phase_completed)){
    # sample new pairs :: need a check to ensure there are new pairs still
    counter <- hash[["count"]]
    stopifnot(is.na(feature_mat[counter+1,1]))
    pairs_mat_new <- .generate_new_pairs(p, so$new_pairs_per_round, hash_pairs)
    
    # extract features of new pairs
    if(verbose) message("MESSAGE")
    feature_mat_tmp <- .extract_features(dat, pairs_mat_new, fl)
    if(any(is.na(feature_mat_tmp))){
      feature_mat_tmp <- .clean_na(feature_mat_tmp, hash_na, offset = counter)
    }
    feature_mat[(counter+1):(counter+nrow(feature_mat_tmp)),] <- feature_mat_tmp
    hash[["count"]] <- counter+nrow(feature_mat_tmp)
    
    pairs_mat <- rbind(pairs_mat, pairs_mat_new)
    
    if(phase_counter == 1){
      
    }
    else if(phase_counter == 2){
      # find which to display :: depends on the phase
      idx <- so$learner_list$second_learner(feature_mat, response_vec, number_requested = 1,
                                            so$learner_options$second_learner)
      pair_vec <- pairs_mat[idx,]
      string_vec <- paste0(pair_vec, collapse = "-")
      stopifnot(hash_pairs[[string_vec]] == idx, is.na(response_vec[idx]))
      
      # display and listen :: depends on the phase
      while(TRUE){
        #plot according to filter
        dat_2col <- cbind(dat[,pair_vec[1]], dat[,pair_vec[2]])
        .plot_dat2col(dat_2col, main = paste0("Plot #",i), ...) # ADD MORE
        
        response <- readline(paste0("Plot #", i, ": Enter 'y' or 'n' on if plot",
                                    " contains patterns of interest: "))
        
        result <- .response_handler(response)
        if(!is.na(result)){response_vec[idx] <- result; break}
      }
    }
  }
  
  
  # learn classifier
  classifier <- system_options$classifier(data = mat, label = vec)
  
  # prepare output
  # RESTORE NA'S
  .construct_dvisR(classifier = classifier)
}

####################

.construct_dvisR <- function(classifier){
  stopifnot(is.function(classifier))
  
  structure(list(classifier = classifier), class = "dvisR")
}

.response_handler <- function(response, filter_idx, col_filter_idx,
                              filter_len, color_filter_len){
  result <- NA
  if(response %in% c("y", "yes", "1", "T", "TRUE")) {
    result <- 1
  } else if(response %in% c("n", "no", "0", "F", "FALSE")){
    result <- 0
  } 
  
  result
}