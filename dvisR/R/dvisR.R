dvisR_system <- function(dat, cluster_labels = rep(NA, nrow(dat)),
                         feature_list = .grab_functions_in_package(),
                         system_options = system_options_default(), 
                         plotting_options = plotting_options_default(),  
                         plotting_module = plotting_module_base,
                         debugging_inputs = NA, verbose = 1, ...){
 
 if(!all(is.na(cluster_labels))) stopifnot(nrow(dat) == length(cluster_labels))
 .check_holistic(dat, plotting_options)
  
 fl <- feature_list; so <- system_options; po <- plotting_options; pm <- plotting_module
 di <- debugging_inputs
 n <- nrow(dat); p <- ncol(dat)
 
 # preprocessing, initialization stuff
 hash_pairs <- .initialize_hash(); hash_na <- .initialize_na_handler()
 feature_mat <- .initialize_feature_matrix(so$ntrials, so$new_pairs_per_round,
                                           so$min_inst, names(fl))
 pairs_mat <- matrix(NA, nrow = 0, ncol = 2)
 response_vec <- rep(NA, nrow(feature_mat))
 round_vec <- rep(NA, nrow(feature_mat))

 round_idx <- 1
 round_phase2_start <- 0
 phase_idx <- 1
 number_requested <- prod(po$first_phase_gridsize)
 
 # enter the main loop of the procedure
 while(!(phase_idx == 2 & round_idx - round_phase2_start > so$ntrials)){
  stopifnot(nrow(feature_mat) == length(response_vec), length(response_vec) == length(round_vec)) 
   
  # sample new pairs
  counter <- hash_pairs[["count"]]
  # check to see if we need to expand feature_mat and others
  if(!is.na(feature_mat[counter+1,1])){
    feature_mat <- .expand_feature_matrix(feature_mat)
    response_vec <- .expand_response_vec(response_vec)
    round_vec <- .expand_response_vec(round_vec)
  }
  pairs_mat_new <- .generate_new_pairs(p, so$new_pairs_per_round[phase_idx], hash_pairs)
  
  # extract features of new pairs
  feature_mat_tmp <- .extract_features(dat, pairs_mat_new, fl)
  if(any(is.na(feature_mat_tmp))){
   feature_mat_tmp <- .clean_na(feature_mat_tmp, hash_na, offset = counter)
  }
  feature_mat[(counter+1):(counter+nrow(feature_mat_tmp)),] <- feature_mat_tmp
  hash_pairs[["count"]] <- counter+nrow(feature_mat_tmp)
  
  pairs_mat <- rbind(pairs_mat, pairs_mat_new)
  
  # do phase 1 learning -- user label multiple plots at once
  if(phase_idx == 1){
   idx_vec <- so$learner_list$first_learner(feature_mat[1:hash_pairs[["count"]],], 
                                            response_vec[1:hash_pairs[["count"]]], 
                                            number_requested = number_requested,
                                            option_list = so$learner_options$first_learner)
   idx_vec <- sort(idx_vec)
   .check_indices(idx_vec, pairs_mat, response_vec, hash_pairs)
   
   ## plot according to filter
   pairs_submat <- pairs_mat[idx_vec,]
   .plotter_first_phase(dat, pairs_submat, plotting_options = po, plotting_module = pm, i = round_idx, 
                        debugging_inputs = di, ...)
   
   ## display and listen
   response_vec[idx_vec] <- .response_listener_first_phase(round_idx, number_requested, debugging_inputs = di)
   round_vec[idx_vec] <- round_idx
  }
  
  # do phase 2 learning -- user labels one plot at a time
  else if(phase_idx == 2){
   # find which to display
   idx <- so$learner_list$second_learner(feature_mat[1:hash_pairs[["count"]],], 
                                         response_vec[1:hash_pairs[["count"]]], 
                                         number_requested = 1,
                                         option_list = so$learner_options$second_learner)
   .check_indices(idx, pairs_mat, response_vec, hash_pairs)
   
   ## plot according to filter
   pair_vec <- pairs_mat[idx,]
   .plotter_second_phase(dat, pair_vec, plotting_options = po, plotting_module = pm, i = round_idx, 
                         offset = round_phase2_start, debugging_inputs = di, ...)
   
   ## display and listen
   response_vec[idx] <- .response_listener_second_phase(round_idx, debugging_inputs = di)
   round_vec[idx] <- round_idx
  }
   
  # cleanup
  if(phase_idx == 1 & length(which(response_vec[1:hash_pairs[["count"]]] == 1)) >= so$min_inst &
     length(which(response_vec[1:hash_pairs[["count"]]] == 0)) >= so$min_inst){
    phase_idx <- 2; round_phase2_start <- round_idx
  }
  round_idx <- round_idx+1
 }
 
 # learn classifier
 classifier <- system_options$classifier(data = feature_mat[which(!is.na(response_vec)),], 
                                         label = response_vec[which(!is.na(response_vec))])
 
 # prepare output
 feature_mat <- .clean_feature_matrix(feature_mat)
 feature_mat <- .restore_na(feature_mat, hash_na)
 response_vec <- .clean_response_vec(response_vec, feature_mat)
 round_vec <- .clean_response_vec(round_vec, feature_mat)
 
 .construct_dvisR(feature_mat, response_vec, pairs_mat, round_vec, classifier, so)
}

####################

.check_holistic <- function(dat, plotting_options){
  if(!all(is.na(plotting_options$color_vec))){
    stopifnot(length(plotting_options$color_vec) == nrow(dat))
  }
  
  stopifnot(is.matrix(dat) | is.data.frame(dat))
  
  invisible()
}

.plotter_first_phase <- function(dat, pairs_submat, plotting_options, plotting_module, i, debugging_inputs = NA, ...){
  if(is.list(debugging_inputs) || all(!is.na(debugging_inputs))) return(invisible())
  
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
                  main = paste0("Round ", i, ": Plot #", j), col = color_vec, ...)
 }
 
 invisible()
}

.plotter_second_phase <- function(dat, pair_vec, plotting_options, plotting_module, i, offset,
                                  debugging_inputs = NA, ...){
  if(is.list(debugging_inputs)  || all(!is.na(debugging_inputs))) return(invisible())
  
  if(!all(is.na(plotting_options$second_mar))){
    graphics::par(mar = plotting_options$second_mar)
  }
  
  graphics::par(mfrow = c(1,1))
  
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
                  main = paste0("Round ", i-offset, " (Total: ", i, ")"), col = color_vec, ...)
  
  invisible()
}

.response_listener_first_phase <- function(i, number_requested, debugging_inputs){
  if((is.list(debugging_inputs)  || all(!is.na(debugging_inputs))) && length(debugging_inputs$round_inputs) >= 1){
    response <- .response_handler_first_phase(debugging_inputs$round_inputs[[i]], number_requested)
    
  } else {
    while(TRUE){
      user_input <- readline(paste0("Round ", i, ": Enter the plot #'s that ",
                                    "contain patterns of interest\n(space-separated): "))
      
      response <- .response_handler_first_phase(user_input, number_requested)
      if(all(!is.na(response))) break
    }
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

.response_listener_second_phase <- function(i, debugging_inputs){
  if((is.list(debugging_inputs)  || all(!is.na(debugging_inputs))) && length(debugging_inputs$round_inputs) >= 1){
    response <- .response_handler_second_phase(debugging_inputs$round_inputs[[i]])
    
  } else {
    while(TRUE){
      user_input <- readline(paste0("Round ", i, ": Enter if the plot ",
                                    "contains patterns of interest\n('y' or 'n'): "))
      
      response <- .response_handler_second_phase(user_input)
      if(all(!is.na(response))) break
    }
  }
 
 response
}


.response_handler_second_phase <- function(user_input){
  if(nchar(user_input) == 0) return(0)
  
  response <- NA
  if(user_input %in% c("y", "yes", "1", "T", "TRUE")) {
    response <- 1
  } else if(user_input %in% c("n", "no", "0", "F", "FALSE")){
    response <- 0
  } 
  
  response
}

#####################

.construct_dvisR <- function(feature_mat, response_vec, pairs_mat, round_vec, 
                             classifier, system_options){
  stopifnot(nrow(feature_mat) == length(response_vec), length(response_vec) == nrow(pairs_mat))
  
  df <- as.data.frame(cbind(pairs_mat, response_vec, round_vec, feature_mat))
  colnames(df) <- c("Idx1", "Idx2", "Response", "Round", colnames(feature_mat))
  
  structure(list(df = df, classifier = classifier), class = "dvisR")
}

.string_parser <- function(str){
  str <- gsub("[[:punct:]]", " ", str)
  str <- gsub("[[:alpha:]]", " ", str)
  str <- unlist(strsplit(str, split = " "))
  str <- str[sapply(str, nchar) > 0]
  
  if(length(str) == 0) return(numeric(0))
  
  as.numeric(str)
}
 
 
 
 