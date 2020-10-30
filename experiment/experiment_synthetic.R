rm(list = ls())
set.seed(10)
n <- 1000; p <- 20
sig_mat <- matrix(0, p, p); sig_mat[1:10,1:10] <- 0.9; sig_mat[11:20,11:20] <- 0.9; diag(sig_mat) <- 1
min(eigen(sig_mat)$values)

dat <- MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = sig_mat)

set.seed(10)
res <- dvisR_system(dat, system_options = system_options_default(ntrials = 10, minimum_instances_first_phase = 4))

# plot the feature-pairs that the system touched
par(mfrow = c(1,2))
dvisR_embedding(res, embedding_method = embedding_umap_closure(), seed = 1, main = "Provided labels",
                legend = embedding_legend(x = "bottomleft", cex = 0.5))
dvisR_embedding(res, embedding_method = embedding_umap_closure(), quantity_plotted = "probability", seed = 1,
                main = "Predicted labels", legend = embedding_legend(x = "bottomleft", cex = 0.5))

# plot all the remaining feature-pairs
pred_res <- dvisR_prediction(res, dat) # this might take a while
par(mfrow = c(1,2))
dvisR_embedding(pred_res, embedding_method = embedding_umap_closure(), seed = 1, main = "Provided labels", 
                legend = embedding_legend(x = "bottomleft", cex = 0.5))
dvisR_embedding(pred_res, embedding_method = embedding_umap_closure(), quantity_plotted = "probability", seed = 1,
                main = "Predicted labels", legend = embedding_legend(x = "bottomleft", cex = 0.5))

# we can also highlight specific variables
par(mfrow = c(1,2))
dvisR_embedding(pred_res, embedding_method = embedding_umap_closure(), highlight_variable = 2, seed = 1, 
                main = "Provided labels", legend = embedding_legend(x = "bottomleft", cex = 0.5))
dvisR_embedding(pred_res, embedding_method = embedding_umap_closure(), quantity_plotted = "probability",
                highlight_variable = 2, seed = 1, main = "Predicted labels", 
                legend = embedding_legend(x = "bottomleft", cex = 0.5))

# plot heatmaps
par(mar = c(0.5, 0.5, 0.5, 0.5), mfrow = c(1,1))
dvisR_heatmap(res)
dvisR_heatmap(res, unlabeled = T)
dvisR_heatmap(pred_res, unlabeled = T)

dvisR_graph(pred_res, cluster_method = clustering_spectral(K = 4))

