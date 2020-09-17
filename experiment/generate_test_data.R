rm(list=ls())
dat <- read.csv("../../data/Zeisel_preprocessed.csv", row.names = 1)
dat <- dat[,1:50]
save(dat, file = "../experiment/test.RData")