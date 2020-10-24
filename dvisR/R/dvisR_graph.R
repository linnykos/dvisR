dvisR_graph <- function(obj, threshold = 0.5, col_palette = col_palette_default(),
            cluster_method = clustering_spectral(K = 4), layout_func = igraph::layout_nicely,
            seed = NA){
  stopifnot(class(obj) == "dvisR_prediction")
  
  if(!is.na(seed)) set.seed(seed)
  adj_mat <- matrix(0, nrow = obj$dim[2], ncol = obj$dim[2])
  for(i in 1:nrow(obj$df)){
    if(obj$probability[i] >= threshold){
      idx1 <- obj$df[i,"Idx1"]; idx2 <- obj$df[i,"Idx2"]
      adj_mat[idx1, idx2] <- 1; adj_mat[idx2, idx1] <- 1
    }
  }
  
  g <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected")
  clustering <- cluster_method(g)
  
  # WARNING: need a way to recycle colors if needed
  igraph::V(g)$color <- col_palette[clustering]
  
  plotting_coord <- layout_func(g)
  graphics::plot(g, layout = plotting_coord)
  
  # WARNING: I want a more graceful to handle the plotting options...
  # WARNING: return the clustering
  invisible()
}

clustering_spectral <- function(K){
  stopifnot(K > 1)

  function(g){
    stopifnot(class(g) == "igraph")
    
    adj_mat <- as.matrix(igraph::as_adjacency_matrix(g))
    eigen_res <- eigen(adj_mat)
    mat <- eigen_res$vectors[,1:K] %*% diag(eigen_res$values[1:K])
    
    stats::kmeans(mat, centers = K)$cluster
  }
}