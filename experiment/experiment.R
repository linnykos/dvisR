rm(list=ls())
load("../experiment/test.RData")
dat <- dat[,1:30]
cell_type <- read.table("../../data/Zeisel_cell_info.txt", sep = "\t", header = 1)
cluster_labels <- as.numeric(as.factor(cell_type$level1class))

set.seed(10)
res <- dvisR::dvisR_system(dat, cluster_labels = cluster_labels, 
                           system_options = system_options_default(minimum_instances_first_phase = 4, ntrials = 10),
                           debugging_inputs = list(round_inputs = list("3,4,5,6", 
                                                                       "n","y","n","y","y",  "y","y","y","y","n")))
res_safe <- res

# plot the feature-pairs that the system touched
par(mfrow = c(1,2))
dvisR_embedding(res, embedding_method = embedding_umap, seed = 1, main = "Provided labels",
                legend = embedding_legend(x = "bottomleft", cex = 0.5))
dvisR_embedding(res, embedding_method = embedding_umap, quantity_plotted = "probability", seed = 1,
                main = "Predicted labels", legend = embedding_legend(x = "bottomleft", cex = 0.5))

# plot all the remaining feature-pairs
pred_res <- dvisR_prediction(res, dat) # this might take a while
par(mfrow = c(1,2))
dvisR_embedding(pred_res, embedding_method = embedding_umap, seed = 1, main = "Provided labels", 
                legend = embedding_legend(x = "bottomleft", cex = 0.5))
dvisR_embedding(pred_res, embedding_method = embedding_umap, quantity_plotted = "probability", seed = 1,
                main = "Predicted labels", legend = embedding_legend(x = "bottomleft", cex = 0.5))

# we can also highlight specific variables
par(mfrow = c(1,2))
dvisR_embedding(pred_res, embedding_method = embedding_umap, highlight_variable = 2, seed = 1, 
                main = "Provided labels", legend = embedding_legend(x = "bottomleft", cex = 0.5))
dvisR_embedding(pred_res, embedding_method = embedding_umap, quantity_plotted = "probability",
                highlight_variable = 2, seed = 1, main = "Predicted labels", 
                legend = embedding_legend(x = "bottomleft", cex = 0.5))

par(mar = c(0.5, 0.5, 0.5, 0.5), mfrow = c(1,1))
dvisR_heatmap(res)
dvisR_heatmap(res, unlabeled = T)

dvisR_graph(pred_res, cluster_method = clustering_spectral(K = 4))

