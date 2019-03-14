learn_df <- subset(combined_df, select = c(Q6_1_1,
                                           Q6_1_2,
                                           Q6_1_3,
                                           Q6_1_4,
                                           Q6_1_5))
learn_df <- cbind(learn_df, cluster = pam_fit$clustering)
learn_df_raw <- learn_df
learn_df <- learn_df %>%
  group_by(cluster) %>%
  summarise_all(sum)

learn_df <-
  cbind(learn_df, size = as.numeric(table(pam_fit$clustering)))

learn_df <- mutate(
  learn_df,
  Q6_1_1_percent = percent(Q6_1_1 / size),
  Q6_1_2_percent = percent(Q6_1_2 / size),
  Q6_1_3_percent = percent(Q6_1_3 / size),
  Q6_1_4_percent = percent(Q6_1_4 / size),
  Q6_1_5_percent = percent(Q6_1_5 / size)
)

learn_df <- learn_df[c(1, 8:12)]

learn_df <- t(learn_df)
learn_df <- as.data.frame(learn_df)
learn_df <- rownames_to_column(learn_df)
learn_df <- learn_df[-1,]
colnames(learn_df) <-
  c("Variable",
    "Cluster 1",
    "Cluster 2",
    "Cluster 3",
    "Cluster 4",
    "Cluster 5")
learn_df[learn_df$Variable == "Q6_1_1_percent", ]$Variable <-
  str_sub(var_label(combined_df$Q6_1_1), 71)
learn_df[learn_df$Variable == "Q6_1_2_percent", ]$Variable <-
  str_sub(var_label(combined_df$Q6_1_2), 71)
learn_df[learn_df$Variable == "Q6_1_3_percent", ]$Variable <-
  str_sub(var_label(combined_df$Q6_1_3), 71)
learn_df[learn_df$Variable == "Q6_1_4_percent", ]$Variable <-
  str_sub(var_label(combined_df$Q6_1_4), 71)
learn_df[learn_df$Variable == "Q6_1_5_percent", ]$Variable <-
  str_sub(var_label(combined_df$Q6_1_5), 71)
