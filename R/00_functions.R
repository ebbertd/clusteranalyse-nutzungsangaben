max_factor <- function(factor) {
  max <- names(which.max(table(factor)))
  per <- percent(prop.table(table(factor))[which.max(prop.table(table(factor)))][[1]])
  pretty <- paste(max, " (", per, ")", sep = "")
  return(pretty)
  return(max)
}

max_logical <- function(logical) {
  max <- as.logical(names(which.max(table(logical))))
  per <- percent(prop.table(table(logical))[which.max(prop.table(table(logical)))][[1]])
  answer <- NA
  if (max == TRUE) {
    answer <- "Yes"
  } else if (max == FALSE) {
    answer <- "No"
  }
  pretty <- paste(answer, " (", per, ")", sep = "")
  pretty
}

mean_numeric <- function(numeric) {
  mean <- round(mean(numeric, na.rm = TRUE), digits = 2)
  sd <- round(sd(numeric, na.rm = TRUE), digits = 2)
  pretty <- paste("(Mean=", mean, ", SD=", sd, ")", sep = "")
  pretty
}

mean_numeric_raw <- function(numeric) {
  mean <- round(mean(numeric, na.rm = TRUE), digits = 3)
  sd <- round(sd(numeric, na.rm = TRUE), digits = 3)
  raw <- paste("Mean=", mean, " (SD=", sd, ")", sep = "")
  raw
}

label_5_1 <- function(mean) {
  mean_round <- as.numeric(str_sub(mean, start = 7, end = 10))
  mean_round <- round(mean_round, digits = 0)
  mean_df <-
    as_tibble(matrix(
      data = NA,
      nrow = length(mean),
      ncol = 2
    ), .name_repair = "minimal")
  colnames(mean_df) <- c("V1", "V2")
  mean_df[1] <- mean_round
  mean_df[2] <- mean
  for (i in 1:nrow(mean_df)) {
    if (mean_df[i, 1] == 1) {
      mean_df[i, 1] <- "no answer"
    } else if (mean_df[i, 1] == 2) {
      mean_df[i, 1] <- "a few"
    } else if (mean_df[i, 1] == 3) {
      mean_df[i, 1] <- "about half"
    } else if (mean_df[i, 1] == 4) {
      mean_df[i, 1] <- "most"
    } else if (mean_df[i, 1] == 5) {
      mean_df[i, 1] <- "almost all"
    }
  }
  mean_df %>%
    transmute(pretty_mean = paste(V1, V2, sep = " "))
}

label_5_6 <- function(mean) {
  mean_round <- as.numeric(str_sub(mean, start = 7, end = 10))
  mean_round <- round(mean_round, digits = 0)
  mean_df <-
    as_tibble(matrix(
      data = NA,
      nrow = length(mean),
      ncol = 2
    ), .name_repair = "minimal")
  colnames(mean_df) <- c("V1", "V2")
  mean_df[1] <- mean_round
  mean_df[2] <- mean
  for (i in 1:nrow(mean_df)) {
    if (mean_df[i, 1] == 1) {
      mean_df[i, 1] <- "no answer"
    } else if (mean_df[i, 1] == 2) {
      mean_df[i, 1] <- "none"
    } else if (mean_df[i, 1] == 3) {
      mean_df[i, 1] <- "a few"
    } else if (mean_df[i, 1] == 4) {
      mean_df[i, 1] <- "about half"
    } else if (mean_df[i, 1] == 5) {
      mean_df[i, 1] <- "most"
    } else if (mean_df[i, 1] == 6) {
      mean_df[i, 1] <- "almost all"
    }
  }
  mean_df %>%
    transmute(pretty_mean = paste(V1, V2, sep = " "))
}

label_6_3 <- function(mean) {
  mean_round <- as.numeric(str_sub(mean, start = 7, end = 10))
  mean_round <- round(mean_round, digits = 0)
  mean_df <-
    as_tibble(matrix(
      data = NA,
      nrow = length(mean),
      ncol = 2
    ), .name_repair = "minimal")
  colnames(mean_df) <- c("V1", "V2")
  mean_df[1] <- mean_round
  mean_df[2] <- mean
  for (i in 1:nrow(mean_df)) {
    if (mean_df[i, 1] == 1) {
      mean_df[i, 1] <- "no answer"
    } else if (mean_df[i, 1] == 2) {
      mean_df[i, 1] <- "always alone"
    } else if (mean_df[i, 1] == 3) {
      mean_df[i, 1] <- "mostly alone"
    } else if (mean_df[i, 1] == 4) {
      mean_df[i, 1] <- "mostly together with other people"
    }
  }
  mean_df %>%
    transmute(pretty_mean = paste(V1, V2, sep = " "))
}
