# Read in WiSe 2016/17 data
wise_2016_17_de <- read.csv2("data/data/wise_2016_17_de.csv")
wise_2016_17_de <- convert_data_types(wise_2016_17_de)
wise_2016_17_de <- add_column(wise_2016_17_de,
                              Language = "DE",
                              Semester = "Winter term 2016/17")

# Read in SoSe 2017 german data
sose_2017_de <- read.csv2("data/data/sose_2017_de.csv")
sose_2017_de <- convert_data_types(sose_2017_de)
sose_2017_de <- add_column(sose_2017_de,
                           Language = "DE",
                           Semester = "Summer term 2017")

# Read in SoSe 2017 english data
sose_2017_en <- read.csv2("data/data/sose_2017_en.csv")
sose_2017_en <- convert_data_types(sose_2017_en)
sose_2017_en <- add_column(sose_2017_en,
                           Language = "EN",
                           Semester = "Summer term 2017")

# Read in WiSe 2017/18 german data
wise_2017_18_de <- read.csv2("data/data/wise_2017_18_de.csv")
wise_2017_18_de <- convert_data_types(wise_2017_18_de)
wise_2017_18_de <- add_column(wise_2017_18_de,
                              Language = "DE",
                              Semester = "Winter term 2017/18")

# Read in WiSe 2017/18 english data
wise_2017_18_en <- read.csv2("data/data/wise_2017_18_en.csv")
wise_2017_18_en <- convert_data_types(wise_2017_18_en)
wise_2017_18_en <- add_column(wise_2017_18_en,
                              Language = "EN",
                              Semester = "Winter term 2017/18")

# Unify factor labels
for (i in 1:ncol(wise_2016_17_de)) {
  if (is.factor(wise_2016_17_de[, i])) {
    factorlist <- list(wise_2016_17_de[, i],
                       sose_2017_de[, i],
                       wise_2017_18_de[, i])
    factorlist <- fct_unify(factorlist)
    wise_2016_17_de[, i] <- factorlist[[1]]
    sose_2017_de[, i] <- factorlist[[2]]
    wise_2017_18_de[, i] <- factorlist[[3]]
  }
}

for (i in 1:ncol(sose_2017_en)) {
  if (is.factor(sose_2017_en[, i])) {
    factorlist <- list(sose_2017_en[, i], wise_2017_18_en[, i])
    factorlist <- fct_unify(factorlist)
    sose_2017_en[, i] <- factorlist[[1]]
    wise_2017_18_en[, i] <- factorlist[[2]]
  }
}

combined_german <-
  rbind(wise_2016_17_de, sose_2017_de, wise_2017_18_de)
combined_english <- rbind(sose_2017_en, wise_2017_18_en)

combined_german[, 3] <- fct_relevel(combined_german[, 3],
                                    "keine Angabe",
                                    "männlich",
                                    "weiblich")
combined_english[, 3] <- fct_relevel(combined_english[, 3],
                                     "no answer",
                                     "male",
                                     "female")

combined_german[, 7] <- fct_relevel(combined_german[, 7],
                                    "keine Angabe",
                                    "Nein",
                                    "Ja")
combined_english[, 7] <- fct_relevel(combined_english[, 7],
                                     "no answer",
                                     "No",
                                     "Yes")

combined_german[, 12] <- fct_relevel(
  combined_german[, 12],
  "keine Angabe",
  "nur wenige",
  "etwa die Hälfte",
  "die meisten",
  "so ziemlich jede"
)
combined_english[, 12] <- fct_relevel(combined_english[, 12],
                                      "no answer",
                                      "a few",
                                      "about half",
                                      "most",
                                      "almost all")

combined_german[, 13] <- fct_relevel(combined_german[, 13],
                                     "keine Angabe",
                                     "Nein",
                                     "Ja")
combined_english[, 13] <- fct_relevel(combined_english[, 13],
                                      "no answer",
                                      "No",
                                      "Yes")

combined_german[, 14] <- fct_relevel(
  combined_german[, 14],
  "Ich habe mir in der Regel nur Teile der Vorlesungsaufzei...",
  "Ich habe mir in der Regel die komplette Vorlesungsaufzei...",
  "Keine Angabe."
)
combined_english[, 14] <- fct_relevel(
  combined_english[, 14],
  "I usually only watch parts of a lecture recording.",
  "I usually watched lecture recordings completely."
)

combined_german[, 15] <- fct_relevel(combined_german[, 15],
                                     "keine Angabe",
                                     "Nein",
                                     "Ja")
combined_english[, 15] <- fct_relevel(combined_english[, 15],
                                      "no answer",
                                      "No",
                                      "Yes")

combined_german[, 16] <- fct_relevel(
  combined_german[, 16],
  "keine Angabe",
  "nur wenige",
  "etwa die Hälfte",
  "die meisten",
  "so ziemlich jede"
)
combined_english[, 16] <- fct_relevel(combined_english[, 16],
                                      "no answer",
                                      "a few",
                                      "about half",
                                      "most",
                                      "almost all")

combined_german[, 32] <- fct_relevel(
  combined_german[, 32],
  "immer alleine",
  "meistens alleine",
  "meistens mit anderen zusammen",
  "keine Angabe"
)
combined_english[, 32] <- fct_relevel(combined_english[, 32],
                                      "always alone",
                                      "mostly alone",
                                      "mostly together with other people")

combined_german[, 33] <- fct_relevel(combined_german[, 33],
                                     "zu Hause",
                                     "in der Universität",
                                     "unterwegs",
                                     "keine Angabe")
combined_english[, 33] <- fct_relevel(combined_english[, 33],
                                      "at home",
                                      "in the university")

for (i in 1:ncol(combined_english[, c(1:33)])) {
  if (is.factor(combined_english[, i])) {
    levels(combined_english[, i]) <- levels(combined_german[, i])
  }
}

# Merge data sets
combined_df <- rbind(combined_german, combined_english)

# Remove unrealistic ages
combined_df$Q2_2[combined_df$Q2_2 < 17] <- NA

# Mean and SD for age
overall_average_age <- mean(combined_df$Q2_2, na.rm = TRUE)
overall_sd_age <- sd(combined_df$Q2_2, na.rm = TRUE)

# Gender distribution
gender_distribution <-
  round(prop.table(table(combined_df$Q2_1)), digits = 4)
gender_distribution <-
  as_tibble(gender_distribution, .name_repair = "minimal")
colnames(gender_distribution) <- c("Gender", "Freq")
gender_distribution[gender_distribution$Gender == "keine Angabe", ]$Gender <-
  "no answer"
gender_distribution[gender_distribution$Gender == "männlich", ]$Gender <-
  "male"
gender_distribution[gender_distribution$Gender == "weiblich", ]$Gender <-
  "female"

# Filter data set
combined_df <- subset(combined_df, Q3_1 != "Nein.")

# Set variable labels
labels_import_english <- read.csv2("data/data/labels_english.csv",
                                   stringsAsFactors = FALSE)
labels_english <- as.list(labels_import_english[, 2])
names(labels_english) <- labels_import_english[, 1]
var_label(combined_df) <- labels_english

combined_df$Q8_1 <- fct_relevel(
  combined_df$Q8_1,
  "sehr unzufrieden",
  "eher unzufrieden",
  "neutral",
  "eher zufrieden",
  "sehr zufrieden"
)

keeps <- c(
  "ID",
  "Timestamp",
  "Q5_1",
  "Q5_2",
  "Q5_4",
  "Q5_5",
  "Q5_6",
  "Q6_3",
  "Q6_4",
  "Language",
  "Semester"
)
filter_df <- combined_df[, keeps]

# Prepare for cluster analysis
filter_df$Q5_1 <- fct_relevel(
  filter_df$Q5_1,
  "keine Angabe",
  "nur wenige",
  "etwa die Hälfte",
  "die meisten",
  "so ziemlich jede"
)
filter_df$Q5_1 <- as.numeric(filter_df$Q5_1)
filter_df$Q5_1[is.na(filter_df$Q5_1)] <- 1

filter_df$Q5_2 <- fct_relevel(filter_df$Q5_2,
                              "keine Angabe",
                              "Nein",
                              "Ja")
filter_df$Q5_2 <- plyr::mapvalues(
  filter_df$Q5_2,
  from = c("keine Angabe",
           "Nein",
           "Ja"),
  to = c("no answer",
         "No",
         "Yes")
)
filter_df$Q5_2[is.na(filter_df$Q5_2)] <- "no answer"

filter_df$Q5_4 <- fct_relevel(
  filter_df$Q5_4,
  "Keine Angabe.",
  "Ich habe mir in der Regel die komplette Vorlesungsaufzei...",
  "Ich habe mir in der Regel nur Teile der Vorlesungsaufzei..."
)
filter_df$Q5_4[is.na(filter_df$Q5_4)] <- "Keine Angabe."
filter_df$Q5_4 <- plyr::mapvalues(
  filter_df$Q5_4,
  from = c(
    "Keine Angabe.",
    "Ich habe mir in der Regel die komplette Vorlesungsaufzei...",
    "Ich habe mir in der Regel nur Teile der Vorlesungsaufzei..."
  ),
  to = c(
    "no answer",
    "I usually watched the lecture recordings completely.",
    "I usually only watch parts of a lecture recording."
  )
)

filter_df$Q5_5[is.na(filter_df$Q5_5)] <- "keine Angabe"
filter_df$Q5_5 <- plyr::mapvalues(
  filter_df$Q5_5,
  from = c("keine Angabe",
           "Nein",
           "Ja"),
  to = c("no answer",
         "No",
         "Yes")
)

filter_df$Q5_6 <- fct_relevel(
  filter_df$Q5_6,
  "keine Angabe",
  "nur wenige",
  "etwa die Hälfte",
  "die meisten",
  "so ziemlich jede"
)
filter_df$Q5_6 <- as.numeric(filter_df$Q5_6)
for (i in 1:length(filter_df$Q5_6)) {
  if (filter_df$Q5_5[i] == "No" & is.na(filter_df$Q5_6[i]) == TRUE) {
    filter_df$Q5_6[i] <- 2
  } else if (is.na(filter_df$Q5_6[i]) == TRUE) {
    filter_df$Q5_6[i] <- 1
  } else if (filter_df$Q5_6[i] == 1) {
    filter_df$Q5_6[i] <- 1
  } else if (filter_df$Q5_6[i] == 2) {
    filter_df$Q5_6[i] <- 3
  } else if (filter_df$Q5_6[i] == 3) {
    filter_df$Q5_6[i] <- 4
  } else if (filter_df$Q5_6[i] == 4) {
    filter_df$Q5_6[i] <- 5
  } else if (filter_df$Q5_6[i] == 5) {
    filter_df$Q5_6[i] <- 6
  }
}

filter_df <- subset(filter_df, select = -c(Q5_5))

filter_df$Q6_3 <- fct_relevel(
  filter_df$Q6_3,
  "keine Angabe",
  "immer alleine",
  "meistens alleine",
  "meistens mit anderen zusammen"
)
filter_df$Q6_3 <- as.numeric(filter_df$Q6_3)
filter_df$Q6_3[is.na(filter_df$Q6_3)] <- 1

filter_df$Q6_4 <- fct_relevel(filter_df$Q6_4,
                              "keine Angabe",
                              "unterwegs",
                              "zu Hause",
                              "in der Universität")
filter_df$Q6_4[is.na(filter_df$Q6_4)] <- "keine Angabe"
filter_df$Q6_4 <- plyr::mapvalues(
  filter_df$Q6_4,
  from = c("keine Angabe",
           "unterwegs",
           "zu Hause",
           "in der Universität"),
  to = c("no answer",
         "on the go",
         "at home",
         "in the university")
)


keeps <- c("Q5_1", "Q5_2", "Q5_4", "Q5_6", "Q6_3", "Q6_4")
cluster_data <- filter_df[, keeps]
cluster_data_backup <- cluster_data
