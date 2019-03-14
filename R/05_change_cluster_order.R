pam_fit$clustering <- pam_fit$clustering + 5
pam_fit$clustering[pam_fit$clustering == 6] <- 1
pam_fit$clustering[pam_fit$clustering == 7] <- 5
pam_fit$clustering[pam_fit$clustering == 8] <- 2
pam_fit$clustering[pam_fit$clustering == 9] <- 4
pam_fit$clustering[pam_fit$clustering == 10] <- 3
