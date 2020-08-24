# existing_pairs is a vector of pairs already looked at. this hash object is passed by reference
.generate_new_pairs <- function(p, number_requested, existing_hash){
  stopifnot(class(existing_hash) == "hash", !is.null(existing_hash[["count"]]))
  
  counter <- 1
  pair_mat <- matrix(NA, ncol = 2, nrow = number_requested)
  
  while(counter < number_requested){
    idx <- sort(sample(p, 2))
    string_vec <- paste0(idx, collapse = "-")
    
    if(is.null(existing_hash[[string_vec]])){
      existing_hash[[string_vec]] <- existing_hash[["count"]]+1
      existing_hash[["count"]] <- existing_hash[["count"]]+1
      
      pair_mat[counter,] <- idx
      counter <- counter+1
    }
  }
  
  list(pair_mat = pair_mat)
}

.initialize_hash <- function(existing_hash){
  stopifnot(class(existing_hash) == "hash")
  
  if(is.null(existing_hash[["count"]])) {
    existing_hash[["count"]] <- 0
  }
  
  invisible()
}