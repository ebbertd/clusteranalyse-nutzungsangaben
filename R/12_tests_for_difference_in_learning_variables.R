difference_Q6_1_1 <-
  chisq.test(
    learn_df_raw$Q6_1_1,
    learn_df_raw$cluster,
    simulate.p.value = TRUE,
    B = 8000
  )
difference_Q6_1_2 <-
  chisq.test(
    learn_df_raw$Q6_1_2,
    learn_df_raw$cluster,
    simulate.p.value = TRUE,
    B = 8000
  )
difference_Q6_1_3 <-
  chisq.test(
    learn_df_raw$Q6_1_3,
    learn_df_raw$cluster,
    simulate.p.value = TRUE,
    B = 8000
  )
difference_Q6_1_5 <-
  chisq.test(
    learn_df_raw$Q6_1_5,
    learn_df_raw$cluster,
    simulate.p.value = TRUE,
    B = 8000
  )

difference_tests_summary_learning_variables <-
  as_tibble(matrix(data = NA, nrow = 4, ncol = 3), .name_repair = "minimal")
colnames(difference_tests_summary_learning_variables) <-
  c("Variable", "Test", "p.value")

difference_tests_summary_learning_variables$Variable[1] <-
  str_sub(var_label(combined_df$Q6_1_1), 71)
difference_tests_summary_learning_variables$Variable[2] <-
  str_sub(var_label(combined_df$Q6_1_2), 71)
difference_tests_summary_learning_variables$Variable[3] <-
  str_sub(var_label(combined_df$Q6_1_3), 71)
difference_tests_summary_learning_variables$Variable[4] <-
  str_sub(var_label(combined_df$Q6_1_5), 71)

difference_tests_summary_learning_variables$Test[1] <-
  difference_Q6_1_1$method
difference_tests_summary_learning_variables$Test[2] <-
  difference_Q6_1_2$method
difference_tests_summary_learning_variables$Test[3] <-
  difference_Q6_1_3$method
difference_tests_summary_learning_variables$Test[4] <-
  difference_Q6_1_5$method

difference_tests_summary_learning_variables$p.value[1] <-
  difference_Q6_1_1$p.value
difference_tests_summary_learning_variables$p.value[2] <-
  difference_Q6_1_2$p.value
difference_tests_summary_learning_variables$p.value[3] <-
  difference_Q6_1_3$p.value
difference_tests_summary_learning_variables$p.value[4] <-
  difference_Q6_1_5$p.value

difference_tests_summary_learning_variables$p.value <-
  round(difference_tests_summary_learning_variables$p.value,
        digits = 6)

difference_tests_summary_learning_variables <-
  difference_tests_summary_learning_variables %>%
  mutate(Significant = p.value < 0.05)

difference_tests_summary_learning_variables$Test <-
  str_replace_all(difference_tests_summary_learning_variables$Test,
                  "[\n\t]" ,
                  "")
