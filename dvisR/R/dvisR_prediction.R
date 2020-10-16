# add parallel backend
dvisR_prediction <- function(obj, dat, ncores = NA){
 # check
 stopifnot(all(dim(dat) == c(obj$dim["n"], obj$dim["p"])))
 p <- obj$dim["p"]
 
 # enumerate the entire df
 res <- .extract_mat_response(obj)
 res_new <- .compute_remaining_feature_mat(obj$df[,c("Idx1", "Idx2")], dat, feature_list = obj$feature_list)
 fm_total <- rbind(res$feature_mat, res_new$feature_mat)
 
 stopifnot(nrow(fm_total) == p*(p-1)/2)
 
 df_new <- as.data.frame(cbind(res_new$pairs_mat, NA, NA, res_new$feature_mat))
 colnames(df_new) <- c("Idx1", "Idx2", "Response", "Round", colnames(res_new$feature_mat))
 
 # apply the classifier
 # can also be parallelized
 prob_vec <-  obj$system_options$classifier$predict(obj$fit_classifier, fm_total)
 pred_vec <- rep("n", length(prob_vec))
 pred_vec[prob_vec >= 1/2] <- "y"
 pred_vec <- as.factor(pred_vec)
 
 # return
 structure(list(df = rbind(obj$df, df_new), probability = prob_vec, prediction = pred_vec),
           class = "dvisR_prediction")
}

####################

.compute_remaining_feature_mat <- function(existing_pairs_mat, dat, feature_list){
 p <- ncol(dat)
 combn_mat <- utils::combn(p, 2)
 
 bool_vec <- sapply(1:ncol(combn_mat), function(j){
  length(intersect(which(existing_pairs_mat$Idx1 %in% combn_mat[1,j]), 
                   which(existing_pairs_mat$Idx2 %in% combn_mat[2,j]))) == 1
 })
 stopifnot(sum(bool_vec) == nrow(existing_pairs_mat))
 new_pairs_mat <- t(combn_mat[,!bool_vec])
 
 feature_mat <- .extract_features(dat, new_pairs_mat, feature_list)
 
 list(pairs_mat = new_pairs_mat, feature_mat = feature_mat)
}