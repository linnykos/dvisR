#' feature Pearson
#'
#' @return function
#' @export
feature_pearson <- function(){
 func <- function(dat){
  abs(stats::cor(dat[,1], dat[,2]))
 }
 
 attr(func, "id") <- "Pearson"
 func
}

#' feature Kendall
#'
#' @return function
#' @export
feature_kendall <- function(){
 func <- function(dat){
  abs(stats::cor(dat[,1], dat[,2], method = "kendall"))
 }
 
 attr(func, "id") <- "Kendall"
 func
}

#' feature Spearman
#'
#' @return non-negative numeric
#' @export
feature_spearman <- function(){
 func <- function(dat){
  abs(stats::cor(dat[,1] - 1e-4*seq_len(nrow(dat)),
                 dat[,2] + 1e-4*seq_len(nrow(dat)),
                 method = "spearman"))
 }
 
 attr(func, "id") <- "Spearman"
 func
}