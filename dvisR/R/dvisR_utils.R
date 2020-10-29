.check_holistic <- function(dat, cluster_labels, feature_list, plotting_options){
   if(!all(is.na(plotting_options$color_vec))){
      stopifnot(length(plotting_options$color_vec) == nrow(dat))
   }
   
   if(!all(is.na(cluster_labels))) stopifnot(length(plotting_options$color_palette) == length(unique(cluster_labels)))
   stopifnot(length(feature_list) > 1, all(sapply(feature_list, is.function)))
   stopifnot(is.matrix(dat) | is.data.frame(dat))
   
   invisible()
}

.extract_mat_response <- function(obj){
 stopifnot(class(obj) %in% c("dvisR", "dvisR_prediction"), "df" %in% names(obj))
 
 response_idx <- which(names(obj$df) == "Response")
 pairs_idx <- which(names(obj$df) %in% c("Idx1", "Idx2"))
 other_idx <-  which(names(obj$df) %in% c("Round"))
 feature_idx <- c(1:ncol(obj$df))[-c(response_idx, pairs_idx, other_idx)]
 
 list(feature_mat = as.matrix(obj$df[,feature_idx]), pairs_mat = as.matrix(obj$df[,pairs_idx]),
      response_vec = as.numeric(obj$df[,response_idx]))
}


.construct_dvisR <- function(feature_mat, response_vec, pairs_mat, cluster_labels,
                             round_vec, n, p, 
                             fit_classifier, feature_list, system_options){
   stopifnot(nrow(feature_mat) == length(response_vec), length(response_vec) == nrow(pairs_mat))
   
   df <- as.data.frame(cbind(pairs_mat, response_vec, round_vec, feature_mat))
   colnames(df) <- c("Idx1", "Idx2", "Response", "Round", colnames(feature_mat))
   dim_vec <- c(n,p); names(dim_vec) <- c("n", "p")
   
   structure(list(df = df, cluster_labels = cluster_labels,
                  fit_classifier = fit_classifier, dim = dim_vec,
                  feature_list = feature_list, system_options = system_options), class = "dvisR")
}

.string_parser <- function(str){
   str <- gsub("[[:punct:]]", " ", str)
   str <- gsub("[[:alpha:]]", " ", str)
   str <- unlist(strsplit(str, split = " "))
   str <- str[sapply(str, nchar) > 0]
   
   if(length(str) == 0) return(numeric(0))
   
   as.numeric(str)
}

.restore_par <- function(par_list){
   graphics::par(mar = par_list$mar)
   graphics::par(mfrow = par_list$mfrow)
   
   invisible()
}


