difference_Q5_1 <- kruskal.test(Q5_1 ~ cluster, data = cluster_data)
difference_Q5_2 <-
  chisq.test(
    cluster_data$Q5_2,
    cluster_data$cluster,
    simulate.p.value = TRUE,
    B = 8000
  )
difference_Q5_4 <-
  chisq.test(
    cluster_data$Q5_4,
    cluster_data$cluster,
    simulate.p.value = TRUE,
    B = 8000
  )
difference_Q5_6 <- kruskal.test(Q5_6 ~ cluster, data = cluster_data)
difference_Q6_3 <- kruskal.test(Q6_3 ~ cluster, data = cluster_data)
difference_Q6_4 <-
  chisq.test(
    cluster_data$Q6_4,
    cluster_data$cluster,
    simulate.p.value = TRUE,
    B = 8000
  )

difference_tests_summary_cluster_variables <-
  as_tibble(matrix(data = NA, nrow = 6, ncol = 3), .name_repair = "minimal")
colnames(difference_tests_summary_cluster_variables) <-
  c("Variable", "Test", "p.value")

difference_tests_summary_cluster_variables$Variable[1] <-
  cluster_results$Variable[1]
difference_tests_summary_cluster_variables$Variable[2] <-
  cluster_results$Variable[2]
difference_tests_summary_cluster_variables$Variable[3] <-
  cluster_results$Variable[3]
difference_tests_summary_cluster_variables$Variable[4] <-
  cluster_results$Variable[4]
difference_tests_summary_cluster_variables$Variable[5] <-
  cluster_results$Variable[5]
difference_tests_summary_cluster_variables$Variable[6] <-
  cluster_results$Variable[6]

difference_tests_summary_cluster_variables$Test[1] <-
  difference_Q5_1$method
difference_tests_summary_cluster_variables$Test[2] <-
  difference_Q5_2$method
difference_tests_summary_cluster_variables$Test[3] <-
  difference_Q5_4$method
difference_tests_summary_cluster_variables$Test[4] <-
  difference_Q5_6$method
difference_tests_summary_cluster_variables$Test[5] <-
  difference_Q6_3$method
difference_tests_summary_cluster_variables$Test[6] <-
  difference_Q6_4$method

difference_tests_summary_cluster_variables$p.value[1] <-
  difference_Q5_1$p.value
difference_tests_summary_cluster_variables$p.value[2] <-
  difference_Q5_2$p.value
difference_tests_summary_cluster_variables$p.value[3] <-
  difference_Q5_4$p.value
difference_tests_summary_cluster_variables$p.value[4] <-
  difference_Q5_6$p.value
difference_tests_summary_cluster_variables$p.value[5] <-
  difference_Q6_3$p.value
difference_tests_summary_cluster_variables$p.value[6] <-
  difference_Q6_4$p.value

difference_tests_summary_cluster_variables$p.value <-
  round(difference_tests_summary_cluster_variables$p.value, digits = 6)

difference_tests_summary_cluster_variables <-
  difference_tests_summary_cluster_variables %>%
  mutate(Significant = p.value < 0.05)

difference_tests_summary_cluster_variables$Test <-
  str_replace_all(difference_tests_summary_cluster_variables$Test,
                  "[\n\t]" ,
                  "")
