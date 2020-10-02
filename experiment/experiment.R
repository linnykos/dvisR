rm(list=ls())
load("../experiment/test.RData")
dat <- dat[,1:30]
cell_type <- read.table("../../data/Zeisel_cell_info.txt", sep = "\t", header = 1)
cluster_labels <- as.numeric(as.factor(cell_type$level1class))

set.seed(10)
res <- dvisR::dvisR_system(dat, cluster_labels = cluster_labels, system_options = system_options_default(minimum_instances_first_phase = 4, ntrials = 10))
res_safe <- res

dvisR_embedding(res, embedding_method = embedding_umap)

par(mar = c(0.5, 0.5, 0.5, 0.5))
dvisR_heatmap(res)
dvisR_heatmap(res, unlabeled = T)

pred_res <- dvisR_prediction(res, dat)

plot(pred_res$probability)
head(pred_res$df)
tail(pred_res$df)
tail(which(pred_res$prediction == 1))
pred_res$df[424,]





