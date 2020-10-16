# WARNING: Add feature_spacing eventually, add feature names, etc.
dvisR_heatmap <- function(obj, reorder_feature = T, predictions = T, unlabeled = F,
                          size_func = heatmap_size_func_default,
                          ylab = NA, par_list = heatmap_par_list_default()){

 res <- .extract_mat_response(obj)
 n <- nrow(res$feature_mat); p <- ncol(res$feature_mat)
 pl <- par_list
 
 # remove unlabeled pairs if needed
 if(!unlabeled){
  nonna_idx <- which(is.na(res$response_vec))
  if(length(nonna_idx) > 0){
   res$feature_mat <- res$feature_mat[-nonna_idx,,drop = F]
   res$response_vec <- res$response_vec[-nonna_idx]
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
  tmp <- obj$system_options$classifier$predict(obj$fit_classifier, res$feature_mat)
  pred_vec <- rep(0, length(tmp))
  pred_vec[tmp >= 1/2] <- 1
 } else {
  pred_vec <- NA
 }
 
 # draw the plot
 .draw_feature_matrix(mat = size_mat, response_vec = res$response_vec, pred_vec = pred_vec,
                      row_order = row_order, col_order = col_order, par_list = par_list)

 
 if(!unlabeled){
   list(row_order = as.numeric(c(1:n)[-nonna_idx][row_order]), col_order = col_order)
 } else {
   list(row_order = as.numeric(row_order), col_order = col_order)
 }
}

heatmap_size_func_default <- function(vec){
 idx <- which(!is.na(vec))
 stopifnot(length(idx) != 0)
 
 mean_val <- mean(vec[idx])
 sd_val <- stats::sd(vec[idx])
 
 if(length(idx) != length(vec)){
  vec[-idx] <- mean_val
 }
 
 size_vec <- (vec - mean_val)/sd_val
 size_vec/max(abs(size_vec))
}

heatmap_par_list_default <- function(col_pos = col_palette_default("teal"),
                                     col_neg = col_palette_default("red"),
                                     col_na = col_palette_default("gray"),
                                     col_big =  col_palette_default("green"),
                                     col_small =  col_palette_default("orange"),
                                     col_bg = col_palette_default("beige"),
                                     col_other = col_palette_default("black"),
                                     max_size = 0.95,
                                     lwd_separator = 2, lty_separator = 1,
                                     lwd_spacing = 0.5, lty_spacing = 2,
                                     cex = 1, response_width = 1){
 list(col_pos = col_pos, col_neg = col_neg, col_na = col_na, col_big = col_big, col_small = col_small, 
      col_bg = col_bg, col_other = col_other,
      max_size = max_size,
      lwd_separator = lwd_separator, lty_separator = 1, lwd_spacing = lwd_spacing, lty_spacing = lty_spacing, 
      cex = cex, response_width = response_width)
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
 unlabel <- ordering[ordering %in% which(is.na(vec))]
 
 tmp <- c(pos, neg, unlabel)
 names(tmp) <- c(rep("1", length(pos)), rep("0", length(neg)), rep("NA", length(unlabel)))
 tmp
}

.draw_feature_matrix <- function(mat, response_vec, pred_vec,
                                 row_order, col_order, par_list){
 
 stopifnot(length(col_order) == ncol(mat), length(row_order) == nrow(mat))
 pl <- par_list
 
 # compute preliminaries
 n <- nrow(mat); p <- ncol(mat)
 if(all(is.na(pred_vec))) xlim <- c(0, p+pl$response_width) else xlim <- c(0, p+2*pl$response_width)
 ylim <- c(0,n)
 
 # compute asp
 
 graphics::plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
 
 usr <- graphics::par("usr")
 
 # plot response and prediction
 .plot_response(response_vec, row_order = row_order, col_order = col_order, par_list = pl, 
                ylim = ylim, offset = 0, width = pl$response_width)
 if(!all(is.na(pred_vec))) .plot_response(pred_vec, row_order = row_order, col_order = col_order, 
                                          ylim = ylim, par_list = pl, offset = pl$response_width,
                                          width = pl$response_width)
 
 # draw background
 offset <- ifelse(!all(is.na(pred_vec)), 2*pl$response_width, 1*pl$response_width)
 graphics::rect(offset, ylim[1], xlim[2], ylim[2], col = pl$col_bg, border = NA)
 
 # plot squares
 mat <- mat[row_order, col_order]
 for(j in 1:p){
  for(i in 1:n){
   half_width <- mat[i,j]*pl$max_size/2
   col <- ifelse(mat[i,j] >= 0, pl$col_big, pl$col_small)
   x_mid <- offset+(j-1)+0.5 
   y_mid <- ylim[2]-(i-1)-0.5 
   graphics::rect(x_mid-half_width, y_mid-half_width, x_mid+half_width, y_mid+half_width, col = col, border = NA)
  }
 }
 
 # add default vertical lines
 graphics::lines(x = rep(pl$response_width, 2), y = ylim,
                 col = pl$col_other, lwd = pl$lwd_separator, lty = pl$lty_separator)
 if(!all(is.na(pred_vec))) graphics::lines(x = rep(2*pl$response_width, 2), y = ylim,
                                 col = pl$col_other, lwd = pl$lwd_separator, lty = pl$lty_separator)
 
 # add horizontal lines
 if(length(names(row_order)) > 1){
  num_pos <- length(which(response_vec == 1))
  graphics::lines(x = xlim, y = rep(ylim[2] - num_pos, 2),
                  col = pl$col_other, lwd = pl$lwd_separator, lty = pl$lty_separator)
  
  if(any(is.na(response_vec))){
    num_neg <- length(which(response_vec == 0))
    graphics::lines(x = xlim, y = rep(ylim[2] - num_pos - num_neg, 2),
                    col = pl$col_other, lwd = pl$lwd_separator, lty = pl$lty_separator)
  }
 }
 
 
 
 invisible()
}

.plot_response <- function(vec, row_order, col_order, par_list, ylim, offset = 0, width = 1){
 pl <- par_list
 
 vec <- vec[row_order]
 
 #draw the response
 for(i in 1:length(row_order)){
  if(is.na(vec[i])){
   col <- pl$col_na
  } else if(vec[i] == 1){
   col <- pl$col_pos
  } else if(vec[i] == 0){
   col <- pl$col_neg
  } else {
   stop()
  }
  
  graphics::rect(offset, ylim[2]-(i-1), offset+width, ylim[2]-i, col = col, border = NA)
 }

 invisible()
}