# stores NAs in a hash
.initialize_na_handler <- function(){
 hash::hash()
}

.clean_na <- function(feature_mat, hash_na, offset = 0){
 idx <- which(is.na(feature_mat), arr.ind = T)
 if(length(idx) == 0) return(feature_mat)
 
 for(i in 1:nrow(idx)){
  hash_na[[as.character(length(hash_na)+1)]] <- c(idx[i,1]+offset, idx[i,2])
  feature_mat[idx[i,1], idx[i,2]] <- 0
 }
 
 feature_mat
}

.restore_na <- function(feature_mat, hash_na){
 if(length(hash_na) == 0) return(feature_mat)
 
 keys <- hash::keys(hash_na)
 for(i in 1:length(keys)){
  idx <- hash_na[[keys[i]]]
  feature_mat[idx[1], idx[2]] <- NA
 }
 
 feature_mat
}