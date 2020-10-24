#' feature Pearson
#'
#' @return function
#' @export
feature_pearson <- function(use_cluster = F){
  func <- function(dat, cluster_labels){
    if(use_cluster){
      vec <- sapply(1:max(cluster_labels), function(k){
        dat_tmp <- dat[which(cluster_labels == k),,drop = F]
        abs(stats::cor(dat_tmp[,1], dat_tmp[,2]))
      })
      
      res <- c(diff(range(vec)), stats::sd(vec))
      names(res) <- c("Pearson_range", "Pearson_std")
    } else {
      res <- abs(stats::cor(dat[,1], dat[,2]))
      names(res) <- "Pearson"
    }
    
    res
  }
  
  func
}

#' feature Kendall
#'
#' @return function
#' @export
feature_kendall <- function(use_cluster = F){
  func <- function(dat, cluster_labels){
    if(use_cluster){
      vec <- sapply(1:max(cluster_labels), function(k){
        dat_tmp <- dat[which(cluster_labels == k),,drop = F]
        abs(stats::cor(dat_tmp[,1], dat_tmp[,2], method = "kendall"))
      })
      
      res <- c(diff(range(vec)), stats::sd(vec))
      names(res) <- c("Kendall_range", "Kendall_std")
    } else {
      res <- abs(stats::cor(dat[,1], dat[,2], method = "kendall"))
      names(res) <- "Kendall"
    }
    
    res
  }
  
  func
}

#' feature Spearman
#'
#' @return non-negative numeric
#' @export
feature_spearman <- function(use_cluster = F){
  func <- function(dat, cluster_labels){
    if(use_cluster){
      vec <- sapply(1:max(cluster_labels), function(k){
        dat_tmp <- dat[which(cluster_labels == k),,drop = F]
        abs(stats::cor(dat_tmp[,1], dat_tmp[,2], method = "spearman"))
      })
      
      res <- c(diff(range(vec)), stats::sd(vec))
      names(res) <- c("Spearman_range", "Spearman_std")
    } else {
      res <- abs(stats::cor(dat[,1], dat[,2], method = "spearman"))
      names(res) <- "Spearman"
    }
    
    res
  }
  
  func
}