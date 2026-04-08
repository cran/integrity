run_checks <- function(dataset, info, alpha = 0.05)
{
  dataset <- .prepare_data(dataset, info)
  images <- list()
  
  # Item 1
  dataset_subset <- dataset[, unlist(info$baseline)]
  check_tables <- list(.repeating_baseline(dataset_subset))
  
  # Item 2
  check_tables <- c(check_tables, list(.repeating_baseline(dataset_subset, "within")))
  
  # Item 3
  rare_columns <- unlist(info$outcome$rare)
  rare_participants <- sapply(dataset[, rare_columns], function(variable) variable != levels(variable)[1])
  rare_participants <- rowSums(rare_participants, na.rm = TRUE) > 0
  dataset_subset <- dataset[rare_participants, ]
  check_tables <- c(check_tables, list(.repeating_baseline(dataset_subset[, unlist(info$baseline)], "across_rare")))
  
  # Item 4
  numeric_columns <- info$baseline$numeric
  numeric_columns <- c(numeric_columns, info$outcome$numeric)
  if(!is.null(numeric_columns))
    images <- list(`Terminal Digits`= .terminal_digits(dataset[, numeric_columns]))
  
  # Item 5
  check_tables <- c(check_tables, list(.excessivelly_homogenous_adjacent(dataset[, info$baseline$dichotomous], alpha = alpha)))
  
  # Item 6
  dataset_subset <- dataset[, c(info$baseline$numeric, info$intervention)]
  check_tables <- c(check_tables, list(.excessivelly_different(dataset_subset, info$intervention, alpha)))
  
  # Item 7
  check_tables <- c(check_tables, list(.excessivelly_different(dataset_subset, info$intervention, alpha, "categorical")))
  
  # Item 8
  check_tables <- c(check_tables, list(.differential_variability(dataset_subset, info$intervention, alpha)))
  
  # Item 9
  results <- .unexpectedly_uncorrelated(dataset_subset, info$correlated, alpha)
  check_tables <- c(check_tables, list(results[["check_table"]]))
  images <- c(images, results[["images"]])
  
  # Items 10 and 13
  check_tables <- c(check_tables, list(.implausible_values(dataset, info$participantID, info$unexpected, info$enrollment)))
  
  # Item 11 a and b
  check_tables <- c(check_tables, list(.excessivelly_homogenous_adjacent(dataset_subset[, info$intervention], "intervention", alpha)))
  plot_data <- dataset
  colnames(plot_data)[match(info$intervention, colnames(plot_data))] <- "intervention"
  colnames(plot_data)[match(info$enrollment$randomisation, colnames(plot_data))] <- "randomisation"
  images <- c(images, list(`Cumulative Allocation` = ggplot2::ggplot(plot_data, ggplot2::aes(.data$randomisation, colour = .data$intervention)) + ggplot2::stat_ecdf() + ggplot2::labs(y = "Empirical Cumulative Distribution")))
  
  # Item 12
  results <- .imbalance_day_intervention(dataset, info$intervention, info$enrollment$randomisation, info$unexpected, alpha)
  check_tables <- c(check_tables, list(results[["check_table"]]))
  images <- c(images, list(Days = results[["image"]]))
  
  # Item 14
  summary_table <- .external_consistency(dataset, info$intervention)
  
  # Item 15
  dataset_missing <- dataset[, -match(c(info$participantID, info$intervention), colnames(dataset))]
  dataset_missing <- lapply(dataset_missing, function(variable) factor(ifelse(is.na(variable), "Missing", "Not Missing"), levels = c("Not Missing", "Missing")))
  dataset_missing$intervention <- dataset[, info$intervention]
  dataset_missing <- do.call(cbind, dataset_missing)
  check_tables <- c(check_tables, list(.external_consistency(dataset_missing, info$intervention, TRUE, alpha)))
  
  check_table_all <- do.call(rbind, check_tables)
  check_table_all <- check_table_all[order(check_table_all$Status), ]
  list(check_table = check_table_all, images = images, summary_table = summary_table)
}