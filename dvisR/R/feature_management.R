# grab all the feaature functions
grab_dependency_functions <- function(package_name = "dvisR",
                                      function_starter = "feature",
                                      num_clusters = NA){
 if(!isNamespaceLoaded(package_name)) stop(paste(package_name, "is not",
                                                 "currently loaded"))
 
 all_obj <- ls(paste0("package:", package_name))
 
 closure_list <- grep(paste0("^", function_starter, "_*"), all_obj, value = T)
 
 # round 1: extracting functions without clustering information
 list1 <- lapply(closure_list, function(x){
  eval(parse(text = paste0(package_name, "::", x)))(use_cluster = F)
 })
 
 # round 2: extracting functions with clustering information
 if(any(!is.na(num_clusters))){
   list2 <- lapply(closure_list, function(x){
     eval(parse(text = paste0(package_name, "::", x)))(use_cluster = T)
   })
 } else {
   list2 <- numeric(0)
 }

  c(list1, list2)
}

# include parallel backend
# extract pairs of columns in pairs, and then apply the features
.extract_features <- function(dat, cluster_labels = rep(1, nrow(dat_2col)), pairs_mat, feature_list){
  stopifnot(ncol(pairs_mat) == 2, max(pairs_mat) <= ncol(dat), 
            all(pairs_mat %% 1 == 0), min(pairs_mat) > 0)
  
  mat_new <- apply(pairs_mat, 1, function(pair){
    dat_2col <- cbind(dat[,pair[1]], dat[,pair[2]])
    
    .apply_feature_list(dat_2col, cluster_labels, feature_list)
  })
  
  t(mat_new)
}

.apply_feature_list <- function(dat_2col, cluster_labels = rep(1, nrow(dat_2col)), 
                                feature_list){
  value_list <- lapply(feature_list, function(feature_func){
    tryCatch({
      feature_func(dat_2col, cluster_labels)
    }, error = function(e){
      val <- NA
      names(val) <- "tmp"
      val
    })
  })
  
  unlist(value_list)
}

####################################

.determine_feature_template <- function(dat, cluster_labels, feature_list){
  .apply_feature_list(dat[,c(1,2)], cluster_labels, feature_list)
}

.initialize_feature_matrix <- function(template_vec, ntrials, new_pairs_per_round,
                                       minimum_instances_first_phase){
  stopifnot(length(new_pairs_per_round) == 2, all(new_pairs_per_round >= 1), all(new_pairs_per_round %% 1 == 0),
            length(ntrials) == 1, ntrials >= 1, ntrials %% 1 == 0,
            length(minimum_instances_first_phase) == 1, minimum_instances_first_phase >= 1, minimum_instances_first_phase %% 1 == 0)
  stopifnot(!is.list(template_vec), is.numeric(template_vec), !is.matrix(template_vec))
  
  est_row <- new_pairs_per_round[1]*minimum_instances_first_phase + new_pairs_per_round[2]*ntrials
  feature_mat <- matrix(NA, nrow = est_row, ncol = length(template_vec))
  colnames(feature_mat) <- names(template_vec)
  
  feature_mat
}

.initial_response_vec <- function(ntrials, new_pairs_per_round,
                                  minimum_instances_first_phase){
  est_row <- new_pairs_per_round[1]*minimum_instances_first_phase + new_pairs_per_round[2]*ntrials
  
  rep(NA, est_row)
}

.expand_feature_matrix <- function(feature_mat, scaling = 1.2){
  stopifnot(is.matrix(feature_mat), scaling > 1)
  
  new_rows <- ceiling((scaling-1) * nrow(feature_mat))
  tmp_mat <- matrix(NA, nrow = new_rows, ncol = ncol(feature_mat))
  colnames(tmp_mat) <- colnames(feature_mat)
  feature_mat <- rbind(feature_mat, tmp_mat)
  stopifnot(is.matrix(feature_mat))
  
  feature_mat
}

.expand_response_vec <- function(response_vec, feature_mat){
  stopifnot(length(response_vec) <= nrow(feature_mat))
  
  if(length(response_vec) < nrow(feature_mat)){
    response_vec <- c(response_vec, rep(NA, nrow(feature_mat) - length(response_vec)))
  }
  
  response_vec
}

.clean_feature_matrix <- function(feature_mat){
  idx <- apply(feature_mat, 1, function(x){!all(is.na(x))})
  feature_mat[idx,,drop = F]
}


.clean_response_vec <- function(response_vec, feature_mat){
  response_vec[1:nrow(feature_mat)]
}

