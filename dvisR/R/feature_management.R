# grab all the feaature functions
.grab_functions_in_package <- function(package_name = "dvisR",
                                       function_starter = "feature"){
 if(!isNamespaceLoaded(package_name)) stop(paste(package_name, "is not",
                                                 "currently loaded"))
 
 all.obj <- ls(paste0("package:", package_name))
 
 fun <- grep(paste0("^", function_starter, "_*"), all.obj, value = T)
 
 lis <- lapply(fun, function(x){
  eval(parse(text = paste0(package_name, "::", x)))
 })
 names(lis) <- fun
 
 lis
}

# extract pairs of columns in pairs, and then apply the features
## SHOULD INCLUDE A SAFETY TRYCATCH
.extract_features <- function(dat, pairs, feature_list){
  mat_new <- apply(pairs, 2, function(x){
    dat_2col <- cbind(dat[,x[1]], dat[,x[2]])
    
    .apply_feature_list(dat_2col, feature_list)
  })
  
  t(mat_new)
}

.apply_feature_list <- function(dat_2col, feature_list){
  res_mat <- sapply(feature_list, function(feature_func){
    feature_func(mat)
  })

  names(res) <- names(feature_list)
  res
}

####################################

.initialize_feature_matrix <- function(ntrials, new_pairs_per_round,
                                       minimum_instances_first_phase, feature_names){
  est_row <- new_pairs_per_round[1]*minimum_instances_first_phase + new_pairs_per_round[2]*ntrials
  mat <- as.data.frame(matrix(NA, nrow = est_row, ncol = length(feature_names)))
  colnames(mat) <- feature_names
  
  mat
}

.update_feature_matrix <- function(mat, remaining_trials, new_pairs_per_round){
  stopifnot(is.data.frame(mat))
  if(any(is.na(mat[,1]))) return(mat)
  
  new_rows <- remaining_trials*new_pairs_per_round
  mat <- rbind(mat, matrix(NA, nrow = new_rows, ncol = ncol(mat)))
  stopifnot(is.data.frame(mat))
  
  mat
}

.clean_feature_matrix <- function(mat){
  idx <- apply(mat, 1, function(x){!all(is.na(mat))})
  mat[idx,,drop = F]
}
