set.seed(10)
n <- 1000; p <- 20
sig_mat <- matrix(0, p, p); sig_mat[1:10,1:10] <- 0.9; sig_mat[11:20,11:20] <- 0.9; diag(sig_mat) <- 1
min(eigen(sig_mat)$values)

dat <- MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = sig_mat)

set.seed(10)
obj <- dvisR_system(dat, system_options = system_options_default(ntrials = 10, minimum_instances_first_phase = 4))
