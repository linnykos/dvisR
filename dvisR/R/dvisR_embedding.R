dvisR_embedding <- function(obj, embedding_method = embedding_pca, highlight_variable = NA,
                            col_palette = col_palette_default(c("gray", "red", "teal")),
                            plotting_module = plotting_module_base, 
                            xlab = "Latent dimension 1", ylab = "Latent dimension 2", 
                            main = "", ...){
  stopifnot(all(is.na(highlight_variable)) || all(is.numeric(highlight_variable)))
  stopifnot(length(col_palette) == 3)
  res <- .extract_mat_response(obj)
  
  embedding_mat <- embedding_method(res$feature_mat)
  stopifnot(ncol(embedding_mat) == 2)
  
  if(all(is.na(highlight_variable))){
    tmp <-  plyr::mapvalues(res$response_vec, from = c(NA, 0, 1), to = c(1, 2, 3))
  } else {
    tmp <- apply(res$pairs_mat, 1, function(x){
      bool <- x %in% highlight_variable
      if(all(bool)){ 3 } else if(all(!bool)){ 1 } else { 2 }
    })
  }
  col_vec <- col_palette[tmp]
  
  ordering <- rep(1, nrow(embedding_mat))
  ordering[!is.na(res$response_vec)] <- 2
  plotting_module(x = embedding_mat[,1], y = embedding_mat[,2], xlab = xlab, ylab = ylab,
                  main = main, col = col_vec, ordering = ordering, ...)
  
  invisible()
}

embedding_pca <- function(mat, ...){
  res <- stats::prcomp(mat, ...)
  res$x[,1:2]
}

embedding_umap <- function(mat, ...){
  uwot::umap(mat, ...)[,1:2]
}