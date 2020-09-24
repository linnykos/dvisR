rm(list=ls())
load("../experiment/test.RData")
cell_type <- read.table("../../data/Zeisel_cell_info.txt", sep = "\t", header = 1)
cluster_labels <- as.numeric(as.factor(cell_type$level1class))

set.seed(10)
res <- dvisR::dvisR_system(dat, 
                            system_options = system_options_default(minimum_instances_first_phase = 4,
                                                                    ntrials = 10, classifier = classifier_xgboost_closure(verbose = 0)),
                            debugging_inputs = list(round_inputs = list("5,6,9", "", "3,5,9,6",
                                                                        "y", "y", "n", "y", "y", "y", "n", "y", "n", "n")))
res_safe <- res


#######

par(mar = c(4,4,4,1))

obj = res_safe
reorder_feature = T
predictions = T
unlabeled = F
size_func = heatmap_size_func_default
prediction_func = predict_xgboost_wrapper
feature_spacing = 5
ylab = NA
par_list = heatmap_par_list_default()

###########

res <- .extract_mat_response(obj)
n <- nrow(res$feature_mat); p <- ncol(res$feature_mat)

# remove unlabeled pairs if needed
if(!unlabeled){
 idx <- which(is.na(res$response_vec))
 if(length(idx) > 0){
  res$feature_mat <- res$feature_mat[-idx,,drop = F]
  res$response_vec <- res$response_vec[-idx]
 }
}

# compute sizes of boxes in the plot
size_mat <- apply(res$feature_mat, 2, size_func)
stopifnot(all(abs(size_mat) <= 1), sum(is.na(size_mat)) == 0)

# compute ordering for the plot
if(reorder_feature){
 col_order <- .hclust_col_ordering(size_mat)
 row_order <- .hclust_row_ordering(size_mat, res$response_vec)
} else {
 col_order <- 1:p; row_order <- 1:n
}

# compute predictions
if(predictions){
 tmp <- prediction_func(obj$classifier, res$feature_mat)
 pred_vec <- rep(0, length(tmp))
 pred_vec[tmp >= 1/2] <- 1
} else {
 pred_vec <- NA
}

# draw the plot
.draw_feature_matrix(mat = size_mat, response_vec = res$response_vec, pred_vec = pred_vec,
                     row_order = row_order, col_order = col_order,
                     col_palette = col_palette, par_list = par_list)




