# given vec (classifications with NA possibly) and mat (features), select an index
.learner_furtherest_distance <- function(mat, vec, number_requested,
                                         option_list){
 stopifnot(nrow(mat) == length(vec), number_requested > 0,
           sum(is.na(vec)) >= number_requested, sum(!is.na(vec)) > 0)
 
 idx_unlabeled <- which(is.na(vec))
 mat_labeled <- mat[-idx_unlabeled,,drop = F]
 mat_unlabeled <- mat[idx_unlabeled,,drop = F]

 dis <- .distance_euclidean(mat, mat_new)
 dis_vec <- apply(dis, 2, function(x){mean(x) - stats::sd(x)})
 idx_unlabeled[order(dis_vec, decreasing = T)[1:number_requested]]
}

.distance_euclidean <- function(mat1, mat2){
 if(ncol(mat1) != ncol(mat2)) stop(paste("mat1 and mat2 must have the same",
                                         "number of columns"))
 
 res <- .rescale_and_separate(mat1, mat2)
 mat1 <- res$mat1; mat2 <- res$mat2
 
 n1 <- nrow(mat1); n2 <- nrow(mat2)
 
 if(n1 < n2){
  mat_small <- mat1; mat_big <- mat2
 } else {
  mat_small <- mat2; mat_big <- mat1
 }
 
 dis <- matrix(0, nrow = nrow(mat_big), ncol = nrow(mat_small))
 for(i in 1:ncol(dis)){
  dis[,i] <- sqrt(rowSums(t((mat_small[i,] - t(mat_big))^2)))
 }
 
 if(n1 < n2) dis <- t(dis)
 
 dis
}

.remove_na_rows <- function(mat){
 mat[which(apply(mat, 1, function(x){all(!is.na(x))})),,drop = F]
}

.rescale_and_separate <- function(mat1, mat2){
 mat1 <- .remove_na_rows(mat1); mat2 <- .remove_na_rows(mat2)
 
 n1 <- nrow(mat1); n2 <- nrow(mat2)
 mat_all <- rbind(mat1, mat2)
 mat_all <- scale(mat_all)
 mat_all[is.nan(mat_all)] <- 0
 mat1 <- mat_all[1:n1,,drop = F]; mat2 <- mat_all[(n1+1):(n1+n2),,drop = F]
 
 list(mat1 = mat1, mat2 = mat2)
}
