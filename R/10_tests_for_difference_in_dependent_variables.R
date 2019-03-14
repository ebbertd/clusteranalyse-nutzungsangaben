difference_Q2_1 <-
  chisq.test(
    dependent_df_raw$Q2_1,
    dependent_df_raw$cluster,
    simulate.p.value = TRUE,
    B = 8000
  )
difference_Q2_2 <-
  kruskal.test(Q2_2 ~ cluster, data = dependent_df_raw)
difference_Q3_1 <-
  chisq.test(
    dependent_df_raw$Q3_1,
    dependent_df_raw$cluster,
    simulate.p.value = TRUE,
    B = 8000
  )
difference_Q4_1 <-
  chisq.test(
    dependent_df_raw$Q4_1,
    dependent_df_raw$cluster,
    simulate.p.value = TRUE,
    B = 8000
  )
difference_Q8_1 <-
  kruskal.test(Q8_1 ~ cluster, data = dependent_df_raw)

difference_tests_summary_dependent_variables <-
  as_tibble(matrix(data = NA, nrow = 5, ncol = 3), .name_repair = "minimal")
colnames(difference_tests_summary_dependent_variables) <-
  c("Variable", "Test", "p.value")

difference_tests_summary_dependent_variables$Variable[1] <-
  dependent_df$Variable[1]
difference_tests_summary_dependent_variables$Variable[2] <-
  dependent_df$Variable[2]
difference_tests_summary_dependent_variables$Variable[3] <-
  dependent_df$Variable[3]
difference_tests_summary_dependent_variables$Variable[4] <-
  dependent_df$Variable[4]
difference_tests_summary_dependent_variables$Variable[5] <-
  dependent_df$Variable[5]

difference_tests_summary_dependent_variables$Test[1] <-
  difference_Q2_1$method
difference_tests_summary_dependent_variables$Test[2] <-
  difference_Q2_2$method
difference_tests_summary_dependent_variables$Test[3] <-
  difference_Q3_1$method
difference_tests_summary_dependent_variables$Test[4] <-
  difference_Q4_1$method
difference_tests_summary_dependent_variables$Test[5] <-
  difference_Q8_1$method

difference_tests_summary_dependent_variables$p.value[1] <-
  difference_Q2_1$p.value
difference_tests_summary_dependent_variables$p.value[2] <-
  difference_Q2_2$p.value
difference_tests_summary_dependent_variables$p.value[3] <-
  difference_Q3_1$p.value
difference_tests_summary_dependent_variables$p.value[4] <-
  difference_Q4_1$p.value
difference_tests_summary_dependent_variables$p.value[5] <-
  difference_Q8_1$p.value

difference_tests_summary_dependent_variables$p.value <-
  round(difference_tests_summary_dependent_variables$p.value,
        digits = 6)

difference_tests_summary_dependent_variables <-
  difference_tests_summary_dependent_variables %>%
  mutate(Significant = p.value < 0.05)

difference_tests_summary_dependent_variables$Test <-
  str_replace_all(difference_tests_summary_dependent_variables$Test,
                  "[\n\t]" ,
                  "")
