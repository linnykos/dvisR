# grab all the feaature functions
.grab_functions_in_package <- function(package_name = "dvisR",
                                       function_starter = "feature"){
 if(!isNamespaceLoaded(package_name)) stop(paste(package_name, "is not",
                                                 "currently loaded"))
 
 all_obj <- ls(paste0("package:", package_name))
 
 fun <- grep(paste0("^", function_starter, "_*"), all_obj, value = T)
 
 lis <- lapply(fun, function(x){
  eval(parse(text = paste0(package_name, "::", x)))()
 })
 
 name_vec <- sapply(lis, function(x){tmp <- attr(x, "id"); if(!is.null(tmp)) tmp else ""})
 
 # handling if nulls appear
 if(any(sapply(name_vec, nchar) == 0)){
   idx <- which(sapply(name_vec, nchar) == 0)
   len_null <- length(grep("NULL*", name_vec[-idx]))
   name_vec[idx] <- paste0("NULL_", (len_null+1):(len_null+length(idx)))
 }
 
 stopifnot(length(unique(name_vec)) == length(name_vec))
 names(lis) <- name_vec
 
 lis
}

# extract pairs of columns in pairs, and then apply the features
.extract_features <- function(dat, pairs_mat, feature_list){
  stopifnot(ncol(pairs_mat) == 2, max(pairs_mat) <= ncol(dat), 
            all(pairs_mat %% 1 == 0), min(pairs_mat) > 0)
  
  mat_new <- apply(pairs_mat, 1, function(pair){
    dat_2col <- cbind(dat[,pair[1]], dat[,pair[2]])
    
    .apply_feature_list(dat_2col, feature_list)
  })
  
  t(mat_new)
}

.apply_feature_list <- function(dat_2col, feature_list){
  feature_vec <- sapply(feature_list, function(feature_func){
    tryCatch({
      feature_func(dat_2col)
    }, error = function(e){
      NA
    })
  })

  names(feature_vec) <- names(feature_list)
  feature_vec
}

####################################

.initialize_feature_matrix <- function(ntrials, new_pairs_per_round,
                                       minimum_instances_first_phase, feature_names){
  est_row <- new_pairs_per_round[1]*minimum_instances_first_phase + new_pairs_per_round[2]*ntrials
  feature_mat <- as.data.frame(matrix(NA, nrow = est_row, ncol = length(feature_names)))
  colnames(feature_mat) <- feature_names
  
  feature_mat
}

.initial_response_vec <- function(ntrials, new_pairs_per_round,
                                  minimum_instances_first_phase, feature_names){
  est_row <- new_pairs_per_round[1]*minimum_instances_first_phase + new_pairs_per_round[2]*ntrials
  rep(NA, est_row)
}

.update_feature_matrix <- function(feature_mat, remaining_trials, new_pairs_per_round){
  stopifnot(is.data.frame(feature_mat))
  if(any(is.na(feature_mat[,1]))) return(feature_mat)
  
  new_rows <- remaining_trials*new_pairs_per_round
  feature_mat <- rbind(feature_mat, matrix(NA, nrow = new_rows, ncol = ncol(feature_mat)))
  stopifnot(is.data.frame(feature_mat))
  
  feature_mat
}

.update_response_vec <- function(response_vec, feature_mat){
  stopifnot(length(response_vec) <= nrow(feature_mat))
  
  if(length(response_vec) < nrow(feature_mat)){
    response_vec <- c(response_vec, rep(NA, nrow(feature_mat) - length(response_vec)))
  }
  
  response_vec
}

.clean_feature_matrix <- function(feature_mat){
  idx <- apply(feature_mat, 1, function(x){!all(is.na(feature_mat))})
  feature_mat[idx,,drop = F]
}
