# works for both dvisR and dvisR_prediction objects
dvisR_embedding <- function(obj, embedding_method = embedding_umap_closure(), highlight_variable = NA,
                            quantity_plotted = "decision",
                            prediction_threshold = .5,
                            col_palette = col_palette_default(c("gray", "red", "teal")),
                            col_levels = 5,
                            plotting_module = plotting_module_base, 
                            seed = NA,
                            legend = embedding_legend("topleft"),
                            xlab = "Latent dimension 1", ylab = "Latent dimension 2", 
                            main = "", ...){
  stopifnot(class(obj) %in% c("dvisR", "dvisR_prediction"))
  stopifnot(quantity_plotted %in% c("decision", "probability", "prediction"))
  
  stopifnot(is.function(legend) || is.na(legend))
  stopifnot(all(is.na(highlight_variable)) || all(is.numeric(highlight_variable)))
  stopifnot(length(col_palette) == 3)
  
  if(!is.na(seed)) set.seed(seed)
  res <- .extract_mat_response(obj)
  
  embedding_mat <- embedding_method(res$feature_mat)
  stopifnot(ncol(embedding_mat) == 2)
  
  # compute predictions, if needed
  ## OPTIMIZE THIS PART (don't need to compute all the predictions)
  if(quantity_plotted %in% c("probability", "prediction")){
    prob_bin <- seq(0, 1, length.out = 2*col_levels)
    prob_midpoint <- sapply(2:length(prob_bin), function(i){mean(prob_bin[c(i-1,i)])})
    
    if(class(obj) == "dvisR_prediction"){
      prob_vec <- obj$probability
    } else {
      prob_vec <- obj$system_options$classifier$predict(obj$fit_classifier, res$feature_mat)
    }
  }
  
  # determine coloring of points
  if(quantity_plotted == "decision"){
    tmp <-  plyr::mapvalues(res$response_vec, from = c(NA, 0, 1), to = c(1, 2, 3))
  } else if(quantity_plotted == "probability") {
    tmp <- sapply(1:length(res$response_vec), function(i){
      which.min(abs(prob_vec[i] - prob_midpoint))
    })
  } else {
    tmp <- plyr::mapvalues(as.numeric((prob_vec >= prediction_threshold)), c(0,1), c(2,3))
  }
  
  # set all points not related to highlight_variable to be gray
  if(any(!is.na(highlight_variable))){
    highlight_idx <- which(apply(res$pairs_mat, 1, function(x){any(x %in% highlight_variable)}))
    if(length(highlight_idx) > 0){
      if(quantity_plotted == "decision"){
        tmp[-highlight_idx] <- 1
      } else {
        tmp[-highlight_idx] <- col_levels
      }
    }
  }
  
  # determine color palette 
  if(quantity_plotted %in% c("decision", "prediction")) {
    col_vec <- col_palette[tmp]
  } else {
    new_palette <- c(grDevices::colorRampPalette(c(col_palette[2], col_palette[1]))(col_levels),
                     grDevices::colorRampPalette(c(col_palette[1], col_palette[3]))(col_levels)[-1])
    col_vec <- new_palette[tmp]
  }
  
  # determine ordering
  ordering <- rep(1, nrow(embedding_mat))
  ordering[!is.na(res$response_vec)] <- 2
  if(any(!is.na(highlight_variable))) {
    if(quantity_plotted == "decision") {
      ordering[intersect(highlight_idx, which(ordering == 2))] <- 3
    } else {
      ordering[highlight_idx] <- 3
    }
  }
  plotting_module(x = embedding_mat[,1], y = embedding_mat[,2], xlab = xlab, ylab = ylab,
                  main = main, col = col_vec, ordering = ordering, ...)
  
  if(is.function(legend)){ legend() }
  
  # SHOULD HAVE A RETURN OBJECT
  invisible()
}

#########################################3

embedding_pca <- function(mat, ...){
  res <- stats::prcomp(mat, ...)
  res$x[,1:2]
}

embedding_umap_closure <- function(...){
  function(mat){
    uwot::umap(mat, ...)[,1:2]
  }
}

embedding_legend <- function(x = "topleft", y = NULL, legend = c("Undecided", "No", "Yes"),
                             fill = col_palette_default(c("gray", "red", "teal")), ...){
  function(){graphics::legend(x = x, y = y, legend = legend, fill = fill, ...)}
}

############################################



