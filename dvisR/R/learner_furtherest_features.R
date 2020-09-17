# given vec (classifications with NA possibly) and mat (features), select an index
learner_furtherest_distance <- function(feature_mat, response_vec, number_requested,
                                         option_list){
 stopifnot(nrow(feature_mat) == length(response_vec), number_requested > 0,
           all(!is.na(feature_mat)),
           sum(is.na(response_vec)) >= number_requested)
 
 idx_unlabeled <- which(is.na(response_vec))
 
 if(length(idx_unlabeled) == length(response_vec)){
   mat <- scale(feature_mat)
   dis <- as.matrix(stats::dist(mat))
     
 } else {
   mat_labeled <- feature_mat[-idx_unlabeled,,drop = F]
   mat_unlabeled <- feature_mat[idx_unlabeled,,drop = F]
   
   res <- .rescale_and_separate(mat_unlabeled, mat_labeled)
   dis <- .distance_euclidean(res$mat1, res$mat2) 
 }
 
 dis_vec <- apply(dis, 1, function(x){mean(x) - stats::sd(x)})
 idx_unlabeled[order(dis_vec, decreasing = T)[1:number_requested]]
}

######################

.distance_euclidean <- function(mat1, mat2){
 if(ncol(mat1) != ncol(mat2)) stop(paste("mat1 and mat2 must have the same",
                                         "number of columns"))
 n1 <- nrow(mat1); n2 <- nrow(mat2)
 
 if(n1 < n2){
  mat_small <- mat1; mat_big <- mat2
 } else {
  mat_small <- mat2; mat_big <- mat1
 }
 
 dis <- matrix(0, nrow = nrow(mat_big), ncol = nrow(mat_small))
 tmat_big <- t(mat_big)
 for(i in 1:ncol(dis)){
  dis[,i] <- sqrt(colSums((mat_small[i,] - tmat_big)^2))
 }
 
 if(n1 < n2) dis <- t(dis)
 
 stopifnot(nrow(dis) == n1, ncol(dis) == n2)
 dis
}

.rescale_and_separate <- function(mat1, mat2){
 
 n1 <- nrow(mat1); n2 <- nrow(mat2)
 mat_all <- rbind(mat1, mat2)
 mat_all <- scale(mat_all)
 mat_all[is.nan(mat_all)] <- 0
 mat1 <- mat_all[1:n1,,drop = F]; mat2 <- mat_all[(n1+1):(n1+n2),,drop = F]
 
 list(mat1 = mat1, mat2 = mat2)
}
