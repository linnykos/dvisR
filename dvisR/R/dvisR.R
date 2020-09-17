dvisR_system <- function(dat, cluster_labels = rep(NA, nrow(dat)),
                         feature_list = .grab_functions_in_package(),
                         system_options = system_options_default(), 
                         plotting_options = plotting_options_default(),  
                         plotting_module = plotting_module_base,
                         debugging_inputs = list(NA), verbose = 1, ...){
 
 if(!is.all(is.na(cluster_labels))) stopifnot(nrow(dat) == length(cluster_labels))
 .check_holistic(dat, plotting_options)
  
 fl <- feature_list; so <- system_options; po <- plotting_options; di <- debugging_inputs; pm <- plotting_module
 p <- ncol(dat)
 
 # preprocessing, initialization stuff
 hash_pairs <- .initialize_hash(); hash_na <- .initialize_na_handler()
 feature_mat <- .initialize_feature_matrix(so$ntrials, so$new_pairs_per_round,
                                           so$minimum_instances_first_phase, names(fl))
 pairs_mat <- matrix(NA, nrow = 0, ncol = 2)
 response_vec <- rep(NA, nrow(feature_mat))

 round_idx <- 1
 phase_counter <- 1
 phase_completed <- rep(FALSE, 2)
 number_requested <- prod(po$first_phase_gridsize)
 
 # enter the main loop of the procedure
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
  
  # do phase 1 learning -- user label multiple plots at once
  if(phase_counter == 1){
   idx_vec <- so$learner_list$first_learner(feature_mat, response_vec, number_requested = number_requested,
                                         options = so$learner_options$first_learner)
   idx_vec <- sort(idx_vec)
   .check_indices(idx_vec, pairs_mat, hash_pairs)
   
   #plot according to filter
   pairs_submat <- pairs_mat[idx_vec,]
   .plotter_first_phase(dat, pairs_submat, plotting_options = po, plotting_module = pm, i = round_idx, ...)
   
   # display and listen
   response_vec[idx_vec] <- .response_listener_first_phase(round_idx, number_requested)
  }
  
  # do phase 2 learning -- user labels one plot at a time
  else if(phase_counter == 2){
   # find which to display
   idx <- so$learner_list$second_learner(feature_mat, response_vec, number_requested = 1,
                                         options = so$learner_options$second_learner)
   .check_indices(idx, pairs_mat, hash_pairs)
   
   #plot according to filter
   pair_vec <- pairs_mat[idx,]
   .plotter_second_phase(dat, pair_vec, plotting_options = po, plotting_module = pm, i = round_idx, ...)
   
   # display and listen
   response_vec[idx] <- .response_listener_second_phase(round_idx)
  }
   
  round_idx <- round_idx+1
 }
 
 # learn classifier
 classifier <- system_options$classifier(data = feature_mat, label = response_vec)
 
 # prepare output
 feature_mat <- .restore_na(feature_mat, hash_na)
 .construct_dvisR(feature_mat, response_vec, classifier, so)
}

####################

.check_holistic <- function(dat, plotting_options){
 if(!all(is.na(plotting_options$color_vec))){
  stopifnot(length(plotting_options$color_vec) == nrow(dat))
 }
 
 invisible()
}

.plotter_first_phase <- function(dat, pairs_submat, plotting_options, plotting_module, i, ...){
 if(!all(is.na(plotting_options$first_mar))){
  graphics::par(mar = plotting_options$first_mar)
 }
 
 graphics::par(mfrow = plotting_options$first_phase_gridsize)
 
 for(j in 1:nrow(pairs_submat)){
  if(plotting_options$axis_labels){
   xlab <- colnames(dat)[pairs_submat[j,1]]; ylab <- colnames(dat)[pairs_submat[j,2]]
  } else {
   xlab <- ""; ylab <- ""
  }
  
  if(length(plotting_options$color_vec) == 1 && all(is.na(plotting_options$color_vec))){
   color_vec <- "black"
  } else{
   color_vec <- plotting_options$color_vec
  }
  
  plotting_module(x = dat[,pairs_submat[j,1]], y = dat[,pairs_submat[j,2]], xlab = xlab, ylab = ylab,
                  main = paste0("Round ", i, ": Plot \#", j), color_vec = color_vec, ...)
 }
 
 invisible()
}

.plotter_second_phase <- function(dat, pair_vec, plotting_options, plotting_module, i, ...){
 if(!all(is.na(plotting_options$second_mar))){
  graphics::par(mar = plotting_options$second_mar)
 }
  
 if(plotting_options$axis_labels){
  xlab <- colnames(dat)[pair_vec[1]]; ylab <- colnames(dat)[pair_vec[2]]
 } else {
  xlab <- ""; ylab <- ""
 }
 
 if(length(plotting_options$color_vec) == 1 && all(is.na(plotting_options$color_vec))){
  color_vec <- "black"
 } else{
  color_vec <- plotting_options$color_vec
 }
 
 plotting_module(x = dat[,pair_vec[1]], y = dat[,pair_vec[2]], xlab = xlab, ylab = ylab,
                 main = paste0("Round ", i), color_vec = color_vec, ...)
 
 invisible()
}

.response_listener_first_phase <- function(i, number_requested){
 while(TRUE){
  user_input <- readline(paste0("Round ", i, ": Enter the plot #'s that ",
                                " contain patterns of interest (space-separated): "))
  
  response <- .response_handler_first_phase(user_input, number_requested)
  if(!is.na(response)) break
 }
 
 response
}

.response_handler_first_phase <- function(user_input, number_requested){
 response <- rep(0, number_requested)
 
 if(nchar(user_input) > 0){
   idx <- .string_parser(user_input)
   
   if(length(idx) > 0){
     stopifnot(all(idx > 0), all(idx <= number_requested), all(idx %% 1 == 0))
     response[idx] <- 1
   }
 }
 
 response
}

.response_listener_second_phase <- function(i){
 while(TRUE){
  user_input <- readline(paste0("Round ", i, ": Enter 'y' or 'n' on if plot",
                                " contains patterns of interest: "))
  
  response <- .response_handler_second_phase(user_input)
  if(!is.na(response)) break
 }
 
 response
}


.response_handler_second_phase <- function(user_input){
 response <- NA
 if(user_input %in% c("y", "yes", "1", "T", "TRUE")) {
  response <- 1
 } else if(user_input %in% c("n", "no", "0", "F", "FALSE")){
  response <- 0
 } 
 
 response
}

 
#####################

.construct_dvisR <- function(feature_mat, response_vec, pairs_mat, classifier, system_options){
  stopifnot(is.function(classifier), nrow(feature_mat) == length(response_vec),
            length(response_vec) == nrow(pairs_mat))
  
  df <- as.data.frame(cbind(pairs_mat, response_vec, feature_mat))
  colnames(df) <- c("Idx1", "Idx2", "Response", colnames(feature_mat))
  
  structure(list(df = df, classifier = classifier), class = "dvisR")
}

.string_parser <- function(str){
  str <- gsub("[[:punct:]]", " ", str)
  str <- gsub("[[:alpha:]]", " ", str)
  str <- unlist(strsplit(str, split = " "))
  str <- str[sapply(str, length) > 0]
  
  if(length(str) == 0) return(numeric(0))
  
  as.numeric(str)
}
 
 
 
 