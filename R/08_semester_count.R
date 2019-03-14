subset_df <- cbind(combined_df, cluster = pam_fit$clustering)
subset_df <- subset_df[c(1, 52, 53)]
semester_count_1 <-
  table(subset(subset_df, subset_df$cluster == 1)$Semester)
semester_count_2 <-
  table(subset(subset_df, subset_df$cluster == 2)$Semester)
semester_count_3 <-
  table(subset(subset_df, subset_df$cluster == 3)$Semester)
semester_count_4 <-
  table(subset(subset_df, subset_df$cluster == 4)$Semester)
semester_count_5 <-
  table(subset(subset_df, subset_df$cluster == 5)$Semester)
semester_count <- rbind(
  semester_count_1,
  semester_count_2,
  semester_count_3,
  semester_count_4,
  semester_count_5
)
semester_count <- as_tibble(semester_count)
semester_count <-
  cbind(semester_count, Sum = rowSums(semester_count))
semester_count <- rbind(semester_count, colSums(semester_count))
semester_count <- mutate(semester_count, Cluster = c(1:5, "Sum"))
semester_count <- semester_count[c(5, 2, 1, 3, 4)]

semester_count <- t(semester_count)
semester_count <- as.data.frame(semester_count)
semester_count <- rownames_to_column(semester_count)
semester_count <- semester_count[-1,]
colnames(semester_count) <-
  c("Semester",
    "Cluster 1",
    "Cluster 2",
    "Cluster 3",
    "Cluster 4",
    "Cluster 5",
    "Sum")
