# existing_pairs is a vector of pairs already looked at. this hash object is passed by reference
.generate_new_pairs <- function(p, new_pairs_per_round, hash_pairs){
  stopifnot(class(hash_pairs) == "hash", !is.null(hash_pairs[["count"]]), 
            length(hash_pairs) + new_pairs_per_round - 1 <= p*(p-1)/2)
  
  counter <- 1
  pairs_mat <- matrix(NA, ncol = 2, nrow = new_pairs_per_round)
  
  while(counter <= new_pairs_per_round){
    pair_vec <- sort(sample(p, 2))
    string_vec <- paste0(pair_vec, collapse = "-")
    
    if(is.null(hash_pairs[[string_vec]])){
      hash_pairs[[string_vec]] <- hash_pairs[["count"]]+1
      hash_pairs[["count"]] <- hash_pairs[["count"]]+1
      
      pairs_mat[counter,] <- pair_vec
      counter <- counter+1
    }
  }
  
  pairs_mat
}

.initialize_hash <- function(){
  hash_obj <- hash::hash()
  
  if(is.null(hash_obj[["count"]])) {
    hash_obj[["count"]] <- 0
  }
  
  hash_obj
}

.check_indices <- function(idx_vec, pairs_mat, response_vec, hash_pairs){
  for(i in 1:length(idx_vec)){
    idx <- idx_vec[i]
    pair_vec <- pairs_mat[idx,]
    string_vec <- paste0(pair_vec, collapse = "-")
    stopifnot(hash_pairs[[string_vec]] == idx, is.na(response_vec[idx]))
  }
  
  invisible()
}