dependent_df <-
  subset(combined_df, select = c(Q2_1, Q2_2, Q3_1, Q4_1, Q8_1))
dependent_df <- cbind(dependent_df, cluster = pam_fit$clustering)

dependent_df$Q2_1 <- plyr::mapvalues(
  dependent_df$Q2_1,
  from = c("keine Angabe",
           "männlich",
           "weiblich"),
  to = c("no answer",
         "male",
         "female")
)

dependent_df$Q3_1 <- plyr::mapvalues(
  dependent_df$Q3_1,
  from = c(
    "Ja, ausschließlich.",
    "Ja, parallel zu den Präsenzveranstaltungen.",
    "Nein."
  ),
  to = c(
    "Yes, I solely used the lecture recordings.",
    "Yes, parallel to the lectures.",
    "No"
  )
)
dependent_df$Q3_1 <- droplevels(dependent_df$Q3_1)

dependent_df$Q4_1 <- plyr::mapvalues(
  dependent_df$Q4_1,
  from = c("keine Angabe",
           "Nein",
           "Ja"),
  to = c("no answer",
         "No",
         "Yes")
)

dependent_df$Q8_1 <- fct_relevel(
  dependent_df$Q8_1,
  "sehr unzufrieden",
  "eher unzufrieden",
  "neutral",
  "eher zufrieden",
  "sehr zufrieden"
)
dependent_df$Q8_1 <- as.numeric(dependent_df$Q8_1)

dependent_df_raw <- dependent_df

dependent_factor <- dependent_df %>%
  group_by(cluster) %>%
  summarise_if(is.factor, max_factor)

dependent_numeric <- dependent_df %>%
  group_by(cluster) %>%
  summarise_if(is.numeric, mean_numeric_raw)

# Add column for cluster size to the data
dependent_df <- cbind(dependent_factor, dependent_numeric[,-1])

dependent_df <- subset(dependent_df, select = c(cluster,
                                                Q2_1,
                                                Q2_2,
                                                Q3_1,
                                                Q4_1,
                                                Q8_1))

dependent_df <- t(dependent_df)
dependent_df <- as.data.frame(dependent_df)
dependent_df <- rownames_to_column(dependent_df)
dependent_df <- dependent_df[-1,]
colnames(dependent_df) <-
  c("Variable",
    "Cluster 1",
    "Cluster 2",
    "Cluster 3",
    "Cluster 4",
    "Cluster 5")
dependent_df[dependent_df$Variable == "Q2_1", ]$Variable <-
  var_label(combined_df$Q2_1)
dependent_df[dependent_df$Variable == "Q2_2", ]$Variable <-
  var_label(combined_df$Q2_2)
dependent_df[dependent_df$Variable == "Q3_1", ]$Variable <-
  var_label(combined_df$Q3_1)
dependent_df[dependent_df$Variable == "Q4_1", ]$Variable <-
  var_label(combined_df$Q4_1)
dependent_df[dependent_df$Variable == "Q8_1", ]$Variable <-
  var_label(combined_df$Q8_1)
