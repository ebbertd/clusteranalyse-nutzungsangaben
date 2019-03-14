cluster_data$Q5_1[cluster_data$Q5_1 == 1] <- NA
cluster_data$Q5_1 <- cluster_data$Q5_1 - 1

cluster_data$Q5_6[cluster_data$Q5_6 == 1] <- NA
cluster_data$Q5_6 <- cluster_data$Q5_6 - 1

cluster_data$Q6_3[cluster_data$Q6_3 == 1] <- NA
cluster_data$Q6_3 <- cluster_data$Q6_3 - 1

cluster_data <- cbind(cluster_data, cluster = pam_fit$clustering)

# Aggregate the data
cluster_results_factor <- cluster_data %>%
  group_by(cluster) %>%
  summarise_if(is.factor, max_factor)

cluster_results_logical <- cluster_data %>%
  group_by(cluster) %>%
  summarise_if(is.logical, max_logical)

cluster_results_numeric <- cluster_data %>%
  group_by(cluster) %>%
  summarise_if(is.numeric, mean_numeric_raw)

cluster_results <- cbind(
  cluster_results_factor,
  cluster_results_logical[,-1],
  cluster_results_numeric[,-1],
  size = as.numeric(table(pam_fit$clustering))
)

cluster_results <- subset(cluster_results,
                          select = c(cluster, Q5_1, Q5_2, Q5_4, Q5_6, Q6_3, Q6_4, size))

cluster_results <- t(cluster_results)
cluster_results <- as.data.frame(cluster_results)
cluster_results <- rownames_to_column(cluster_results)
cluster_results <- cluster_results[-1,]
colnames(cluster_results) <-
  c("Variable",
    "Cluster 1",
    "Cluster 2",
    "Cluster 3",
    "Cluster 4",
    "Cluster 5")
cluster_results[cluster_results$Variable == "Q5_1", ]$Variable <-
  var_label(combined_df$Q5_1)
cluster_results[cluster_results$Variable == "Q5_2", ]$Variable <-
  var_label(combined_df$Q5_2)
cluster_results[cluster_results$Variable == "Q5_4", ]$Variable <-
  var_label(combined_df$Q5_4)
cluster_results[cluster_results$Variable == "Q5_6", ]$Variable <-
  var_label(combined_df$Q5_6)
cluster_results[cluster_results$Variable == "Q6_3", ]$Variable <-
  var_label(combined_df$Q6_3)
cluster_results[cluster_results$Variable == "Q6_4", ]$Variable <-
  var_label(combined_df$Q6_4)
cluster_results[cluster_results$Variable == "size", ]$Variable <-
  "Size"
