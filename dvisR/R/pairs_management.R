# existing_pairs is a vector of pairs already looked at
.generate_new_pairs <- function(p, number_requested, existing_hash){
 counter <- 1
 pair_mat <- matrix(NA, ncol = 2, nrow = number_requested)
 
 while(counter < number_requested){
  idx <- sort(sample(p, 2))
  string_vec <- paste0(idx, collapse = "-")
  
  if(is.null(existing_hash[[string_vec]])){
   existing_hash[[string_vec]] <- TRUE
   pair_mat[counter,] <- idx
   counter <- counter+1
  }
 }
 
 list(pair_mat = pair_mat, existing_hash = existing_hash)
}