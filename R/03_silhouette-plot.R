sil_width <- c(NA)

for (i in 2:10) {
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot the silhouette plot
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
axis(side = 1,
     at = seq(1:10),
     lwd = 3)

sil_width_df <-
  as_tibble(matrix(data = NA, nrow = 10, ncol = 2), .name_repair = "minimal")
colnames(sil_width_df) <-
  c("Number of clusters", "Silhouette Width")
sil_width_df$`Number of clusters` <- c(1:10)
sil_width_df$`Silhouette Width` <- round(sil_width, digits = 2)
