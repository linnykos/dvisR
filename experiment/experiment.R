rm(list=ls())
load("../experiment/test.RData")
cell_type <- read.table("../../data/Zeisel_cell_info.txt", sep = "\t", header = 1)
cluster_labels <- as.numeric(as.factor(cell_type$level1class))

set.seed(10)
res <- dvisR_system(dat, 
                    system_options = system_options_default(minimum_instances_first_phase = 4,
                                                            ntrials = 10, classifier = classifier_xgboost_closure(verbose = 0)),
                    plotting_options = plotting_options_default(color_vec = cluster_labels), 
                    pch = 16, asp = T)