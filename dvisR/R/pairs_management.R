# existing_pairs is a vector of pairs already looked at. this hash object is passed by reference
.generate_new_pairs <- function(p, new_pairs_per_round, existing_hash){
  stopifnot(class(existing_hash) == "hash", !is.null(existing_hash[["count"]]), 
            length(existing_hash) + new_pairs_per_round - 1 <= p*(p-1)/2)
  
  counter <- 1
  pairs_mat <- matrix(NA, ncol = 2, nrow = new_pairs_per_round)
  
  while(counter <= new_pairs_per_round){
    pair_vec <- sort(sample(p, 2))
    string_vec <- paste0(pair_vec, collapse = "-")
    
    if(is.null(existing_hash[[string_vec]])){
      existing_hash[[string_vec]] <- existing_hash[["count"]]+1
      existing_hash[["count"]] <- existing_hash[["count"]]+1
      
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