dvisR_heatmap <- function(obj, reorder_feature = T, predictions = T, unlabeled = T,
                          size_func = heatmap_size_func_default,
                          prediction_func = predict_xgboost_wrapper,
                          feature_spacing = 5, ylab = NA,
                          par_list = heatmap_par_list_default()){
 
 res <- .extract_mat_response(obj)
 n <- nrow(res$feature_mat); p <- ncol(res$feature_mat)
 pl <- par_list
 
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
 
 #add vertical lines
 idx <- sum(x$response)
 row.spacing <- 1/(n-1)
 graphics::lines(x = c(-1,2), y = rep(idx*row.spacing - row.spacing/2, 2), lwd = 4)
 
 #add horizontal lines
 col.spacing <- 1/d
 graphics::lines(x = rep(col.spacing - col.spacing/2, 2), y = c(-1, 2), lwd = 4)
 
 list(col.order = colnames(x$feature.matrix)[col.order],
      row.order = row.order)
}

heatmap_size_func_default <- function(vec){
 idx <- which(!is.na(vec))
 stopifnot(length(idx) != 0)
 
 mean_val <- mean(vec[idx])
 sd_val <- sd(vec[idx])
 
 if(length(idx) != length(vec)){
  vec[-idx] <- mean_val
 }
 
 size_vec <- (vec - mean_val)/sd_val
 size_vec/max(abs(size_vec))
}

heatmap_par_list_default <- function(col_pos = col_palette_default("green"),
                                     col_neg = col_palette_default("orange"),
                                     col_na = col_palette_default("gray"),
                                     col_big =  col_palette_default("teal"),
                                     col_small =  col_palette_default("red"),
                                     col_bg = col_palette_default("beige"),
                                     col_other = col_palette_default("black"),
                                     max_size = 0.95,
                                     lwd_separator = 2, lty_separator = 1,
                                     lwd_spacing = 0.5, lty_spacing = 2,
                                     cex = 1){
 list(col_pos = col_pos, col_neg = col_neg, col_na = col_na, col_big = col_big, col_small = col_small, 
      col_bg = col_bg, col_other = col_other,
      max_size = max_size,
      lwd_separator = lwd_separator, lty_separator = 1, lwd_spacing = lwd_spacing, lty_spacing = lty_spacing, cex = cex)
}

##############################

.hclust_col_ordering <- function(mat){
 mat <- scale(mat)
 res <- stats::hclust(stats::dist(t(mat)))
 
 res$order
}

.hclust_row_ordering <- function(mat, vec){
 mat <- scale(mat)
 res <- stats::hclust(stats::dist(mat))
 
 ordering <- res$order
 
 pos <- ordering[ordering %in% which(vec == 1)]
 neg <- ordering[ordering %in% which(vec == 0)]
 unlabel <- ordering[which(is.na(vec))]
 
 tmp <- c(pos, neg, unlabel)
 names(tmp) <- c(rep("1", length(pos)), rep("0", length(neg)), rep("NA", length(unlabel)))
 tmp
}

.draw_feature_matrix <- function(mat, response_vec, pred_vec,
                                 row_order, col_order, 
                                 col_palette, par_list){
 
 stopifnot(length(col_order) == ncol(mat), length(row_order) == nrow(mat))
 pl <- par_list
 
 # compute preliminaries
 n <- nrow(mat); p <- ncol(mat)
 if(all(is.na(pred_vec))) xlim <- c(0, p+1) else xlim <- c(0, p+2)
 ylim <- c(0,n)
 
 # compute asp
 
 graphics::plot(NA, xlim = xlim, ylim = ylim) #, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
 
 # usr <- graphics::par("usr")
 # if(abs(usr[1]-usr[2]) < abs(usr[3]-usr[4])){
 #  graphics::par(usr = c(xlim[1], xlim[2], usr[3], usr[4]))
 # } else {
 #  graphics::par(usr = c(usr[1], usr[2], ylim[1], ylim[2]))
 # }
 
 # plot response and prediction
 .plot_response(response_vec, row_order = row_order, col_order = col_order, par_list = pl, 
                ylim = ylim, offset = 0)
 if(!all(is.na(pred_vec))) .plot_response(pred_vec, row_order = row_order, col_order = col_order, 
                                          ylim = ylim, par_list = pl, offset = 1)
 
 # draw background
 offset <- ifelse(!all(is.na(pred_vec)), 2, 1)
 graphics::rect(offset, ylim[1], xlim[2], ylim[2], col = pl$col_bg, border = NA)
 
 # plot squares
 mat <- mat[row_order, col_order]
 for(j in 1:p){
  for(i in 1:n){
   half_width <- mat[i,j]*pl$max_size/2
   col <- ifelse(mat[i,j] >= 0, pl$col_big, pl$col_small)
   x_mid <- offset+(j-1)+0.5 
   y_mid <- ylim[2]-(i-1)-0.5 
   print(x_mid)
   print(y_mid)
   print("----")
   graphics::rect(x_mid-half_width, y_mid-half_width, x_mid+half_width, y_mid+half_width, col = col, border = NA)
  }
 }
 
 invisible()
}

.plot_response <- function(vec, row_order, col_order, par_list, ylim, offset = 0){
 pl <- par_list
 
 #draw the response
 for(i in 1:length(row_order)){
  if(is.na(vec[row_order[i]])){
   col <- pl$col_na
  } else if(vec[row_order[i]] == 1){
   col <- pl$col_pos
  } else if(vec[row_order[i]] == 0){
   col <- pl$col_neg
  } else {
   stop()
  }
  
  graphics::rect(offset, ylim[2]-(i-1), offset+1, ylim[2]-i, col = col, border = NA)
 }

 invisible()
}