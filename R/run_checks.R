.info_columns <- function(columns)
{
  columns <- unlist(columns, use.names = FALSE)
  columns[!is.na(columns) & nzchar(columns)]
}

.has_columns <- function(dataset, columns)
{
  columns <- .info_columns(columns)
  length(columns) > 0 && all(columns %in% colnames(dataset))
}

.available_columns <- function(dataset, columns)
{
  columns <- .info_columns(columns)
  columns[columns %in% colnames(dataset)]
}

.is_date_column <- function(dataset, column)
{
  .has_columns(dataset, column) && inherits(dataset[[column]], c("Date", "POSIXt"))
}

.categorical_variables_with_variance <- function(dataset, columns)
{
  columns <- .info_columns(columns)
  columns <- columns[columns %in% colnames(dataset)]
  if(length(columns) == 0) return(list(kept = character(), removed = character()))
  
  non_single_level <- vapply(columns, function(column)
  {
    length(unique(stats::na.omit(dataset[[column]]))) >= 2
  }, logical(1))
  
  list(
    kept = columns[non_single_level],
    removed = columns[!non_single_level]
  )
}

.numeric_variables_with_variance <- function(dataset, columns)
{
  columns <- .info_columns(columns)
  columns <- columns[columns %in% colnames(dataset)]
  if(length(columns) == 0) return(list(kept = character(), removed = character()))
  
  non_constant <- vapply(columns, function(column)
  {
    length(unique(stats::na.omit(dataset[[column]]))) >= 2
  }, logical(1))
  
  list(
    kept = columns[non_constant],
    removed = columns[!non_constant]
  )
}

.append_removed_variable_message <- function(check_table, removed)
{
  if(length(removed) == 0) return(check_table)
  
  removed_text <- paste(paste0("`", removed, "`"), collapse = ", ")
  note <- paste(
    "Removed single-level categorical variable",
    if(length(removed) > 1) "s" else "",
    removed_text,
    "because only one non-missing level was present."
  )
  check_table[["Details"]] <- paste(check_table[["Details"]], note)
  check_table
}

.append_removed_numeric_variable_message <- function(check_table, removed)
{
  if(length(removed) == 0) return(check_table)
  
  removed_text <- paste(paste0("`", removed, "`"), collapse = ", ")
  note <- paste(
    "Removed numeric variable",
    if(length(removed) > 1) "s" else "",
    removed_text,
    "because fewer than two distinct non-missing values were present."
  )
  check_table[["Details"]] <- paste(check_table[["Details"]], note)
  check_table
}

.append_all_missing_variable_message <- function(check_table, removed)
{
  if(length(removed) == 0) return(check_table)
  
  removed_text <- paste(paste0("`", removed, "`"), collapse = ", ")
  note <- paste(
    "Removed variable",
    if(length(removed) > 1) "s" else "",
    removed_text,
    "because all values were missing."
  )
  check_table[["Details"]] <- paste(check_table[["Details"]], note)
  check_table
}

.skip_no_available_columns <- function(result_item, columns, removed, available_message, removed_message)
{
  if(length(.info_columns(columns)) > 0 && length(removed) > 0)
    return(.skip_check(result_item, removed_message))
  
  .skip_check(result_item, available_message)
}

run_checks <- function(dataset, info, alpha = 0.05)
{
  all_missing_columns <- names(dataset)[vapply(dataset, function(column) all(is.na(column)), logical(1))]
  dataset <- .prepare_data(dataset, info)
  images <- list()
  check_tables <- list()
  detail_tables <- list()
  
  participant_id <- info$participantID
  intervention <- info$intervention
  has_intervention <- .has_columns(dataset, intervention)
  intervention_levels_n <- if(has_intervention) length(unique(stats::na.omit(dataset[[intervention]]))) else 0
  has_comparison_groups <- has_intervention && intervention_levels_n >= 2
  has_randomisation <- .is_date_column(dataset, info$enrollment$randomisation)
  randomisation_missing_message <- "Randomisation date was not nominated so we cannot evaluate this item."
  baseline_columns <- .available_columns(dataset, info$baseline)
  baseline_removed_all_missing <- intersect(.info_columns(info$baseline), all_missing_columns)
  numeric_baseline <- .available_columns(dataset, info$baseline$numeric)
  numeric_baseline_removed_all_missing <- intersect(.info_columns(info$baseline$numeric), all_missing_columns)
  categorical_baseline <- .available_columns(dataset, info$baseline[c("dichotomous", "polytomous")])
  rare_columns <- .available_columns(dataset, info$outcome$rare)
  rare_removed_all_missing <- intersect(.info_columns(info$outcome$rare), all_missing_columns)
  outcome_columns <- .available_columns(dataset, info$outcome)
  correlated_pairs <- info$correlated
  correlated_pairs <- correlated_pairs[vapply(correlated_pairs, function(pair) all(.info_columns(pair) %in% colnames(dataset)), logical(1))]
  correlated_columns <- .available_columns(dataset, correlated_pairs)
  dichotomous_event_columns <- .available_columns(dataset, c(info$baseline$dichotomous, info$outcome$common$dichotomous, info$outcome$rare$dichotomous))
  numeric_outcome_columns <- .available_columns(dataset, c(
    info$outcome$numeric,
    info$outcome$continuous,
    info$outcome$common$numeric,
    info$outcome$common$continuous,
    info$outcome$rare$numeric,
    info$outcome$rare$continuous
  ))
  numeric_columns <- .available_columns(dataset, c(info$baseline$numeric, numeric_outcome_columns))
  numeric_removed_all_missing <- intersect(.info_columns(c(info$baseline$numeric, info$outcome$numeric, info$outcome$continuous, info$outcome$common$numeric, info$outcome$common$continuous, info$outcome$rare$numeric, info$outcome$rare$continuous)), all_missing_columns)
  summary_columns <- unique(c(intervention, baseline_columns, outcome_columns))
  numeric_baseline_check <- .numeric_variables_with_variance(dataset, numeric_baseline)
  categorical_baseline_check <- .categorical_variables_with_variance(dataset, categorical_baseline)
  dichotomous_event_check <- .categorical_variables_with_variance(dataset, dichotomous_event_columns)
  
  # Item 1
  if(length(baseline_columns) > 0)
  {
    baseline_subset <- dataset[, baseline_columns, drop = FALSE]
    check_result <- .repeating_baseline(baseline_subset)
    check_result <- .append_all_missing_variable_message(check_result, baseline_removed_all_missing)
    check_tables <- c(check_tables, list(check_result))
  } else {
    check_tables <- c(check_tables, list(.skip_no_available_columns("Repeated Baselines", info$baseline, baseline_removed_all_missing, "No baseline variables were provided.", "All nominated baseline variables were removed because all values were missing.")))
  }
  
  # Item 2
  if(length(baseline_columns) > 0)
  {
    check_result <- .repeating_baseline(baseline_subset, "within")
    check_tables <- c(check_tables, list(check_result))
  }
  else
    check_tables <- c(check_tables, list(.skip_no_available_columns("Repeated Baselines Within Variables", info$baseline, baseline_removed_all_missing, "No baseline variables were provided.", "All nominated baseline variables were removed because all values were missing.")))
  
  # Item 3
  if(length(baseline_columns) > 0 && length(rare_columns) > 0)
  {
    rare_participants <- sapply(dataset[, rare_columns, drop = FALSE], function(variable)
    {
      variable <- factor(variable)
      variable != levels(variable)[1]
    })
    rare_participants <- rowSums(rare_participants, na.rm = TRUE) > 0
    dataset_subset <- dataset[rare_participants, , drop = FALSE]
    check_result <- .repeating_baseline(dataset_subset[, baseline_columns, drop = FALSE], "across_rare")
    check_result <- .append_all_missing_variable_message(check_result, c(baseline_removed_all_missing, rare_removed_all_missing))
    check_tables <- c(check_tables, list(check_result))
  } else {
    check_tables <- c(check_tables, list(.skip_no_available_columns("Repeated Baselines in Rare Outcomes", info$outcome$rare, rare_removed_all_missing, "No rare outcome variables were provided.", "All nominated rare outcome variables were removed because all values were missing.")))
  }
  
  # Item 4
  if(length(numeric_columns) > 0)
  {
    images <- list(`Terminal Digits` = .terminal_digits(dataset[, numeric_columns, drop = FALSE]))
    check_result <- .check_result("Terminal Digits", "Displayed", "Terminal digit plot generated.")
    check_result <- .append_all_missing_variable_message(check_result, numeric_removed_all_missing)
    check_tables <- c(check_tables, list(check_result))
  }
  else
    check_tables <- c(check_tables, list(.skip_no_available_columns("Terminal Digits", c(info$baseline$numeric, info$outcome$numeric, info$outcome$continuous, info$outcome$common$numeric, info$outcome$common$continuous, info$outcome$rare$numeric, info$outcome$rare$continuous), numeric_removed_all_missing, "No continuous variables were provided for terminal digit plots.", "All nominated continuous variables were removed because all values were missing.")))
  
  # Item 5
  dichotomous_baseline <- .available_columns(dataset, info$baseline$dichotomous)
  if(length(dichotomous_baseline) > 0)
  {
    dataset_subset <- dataset[, dichotomous_baseline, drop = FALSE]
    check_tables <- c(check_tables, list(.excessivelly_homogenous_adjacent(dataset_subset, alpha = alpha)))
    detail_tables[["2.1"]] <- .detail_excessivelly_homogenous_adjacent(dataset_subset, alpha)
  } else
    check_tables <- c(check_tables, list(.skip_check("Consecutive Baseline Binary", "No dichotomous baseline variables were provided.")))
  
  # Item 6
  if(length(numeric_baseline_check[["kept"]]) > 0 && has_comparison_groups)
  {
    dataset_subset <- dataset[, c(numeric_baseline_check[["kept"]], intervention), drop = FALSE]
    check_result <- .excessivelly_different(dataset_subset, intervention, alpha)
    check_result <- .append_all_missing_variable_message(check_result, numeric_baseline_removed_all_missing)
    check_result <- .append_removed_numeric_variable_message(check_result, numeric_baseline_check[["removed"]])
    check_tables <- c(check_tables, list(check_result))
    detail_tables[["2.2"]] <- .detail_excessivelly_different(dataset_subset, intervention, "numeric")
  } else {
    if(length(numeric_baseline) == 0 && length(numeric_baseline_removed_all_missing) > 0)
      check_tables <- c(check_tables, list(.skip_check("Excessive Imbalances (Numeric)", "All nominated numeric baseline variables were removed because all values were missing.")))
    else if(length(numeric_baseline_check[["removed"]]) > 0)
      check_tables <- c(check_tables, list(.skip_check("Excessive Imbalances (Numeric)", "All available numeric baseline variables were removed because fewer than two distinct non-missing values were present.")))
    else if(!has_comparison_groups)
      check_tables <- c(check_tables, list(.skip_check("Excessive Imbalances (Numeric)", "Numeric baseline variables and an intervention variable with at least two non-missing levels are required.")))
    else
      check_tables <- c(check_tables, list(.skip_check("Excessive Imbalances (Numeric)", "Numeric baseline variables and an intervention variable are required.")))
  }
  
  # Item 7
  if(length(categorical_baseline_check[["kept"]]) > 0 && has_comparison_groups)
  {
    dataset_subset <- dataset[, c(categorical_baseline_check[["kept"]], intervention), drop = FALSE]
    check_result <- .excessivelly_different(dataset_subset, intervention, alpha, "categorical")
    check_result <- .append_removed_variable_message(check_result, categorical_baseline_check[["removed"]])
    check_tables <- c(check_tables, list(check_result))
    detail_tables[["2.3"]] <- .detail_excessivelly_different(dataset_subset, intervention, "categorical")
  } else if(length(categorical_baseline_check[["removed"]]) > 0 && has_comparison_groups) {
    check_tables <- c(check_tables, list(.skip_check("Excessive Imbalances (Categorical)", "All nominated categorical baseline variables were removed because only one non-missing level was present.")))
  } else if(!has_comparison_groups) {
    check_tables <- c(check_tables, list(.skip_check("Excessive Imbalances (Categorical)", "Categorical baseline variables and an intervention variable with at least two non-missing levels are required.")))
  } else {
    check_tables <- c(check_tables, list(.skip_check("Excessive Imbalances (Categorical)", "Categorical baseline variables and an intervention variable are required.")))
  }
  
  # Item 8
  if(length(numeric_baseline_check[["kept"]]) > 0 && has_comparison_groups)
  {
    dataset_subset <- dataset[, c(numeric_baseline_check[["kept"]], intervention), drop = FALSE]
    check_result <- .differential_variability(dataset_subset, intervention, alpha)
    check_result <- .append_all_missing_variable_message(check_result, numeric_baseline_removed_all_missing)
    check_result <- .append_removed_numeric_variable_message(check_result, numeric_baseline_check[["removed"]])
    check_tables <- c(check_tables, list(check_result))
    detail_tables[["2.4"]] <- .detail_differential_variability(dataset_subset, intervention, alpha)
  } else {
    if(length(numeric_baseline) == 0 && length(numeric_baseline_removed_all_missing) > 0)
      check_tables <- c(check_tables, list(.skip_check("Differential Variability", "All nominated numeric baseline variables were removed because all values were missing.")))
    else if(length(numeric_baseline_check[["removed"]]) > 0)
      check_tables <- c(check_tables, list(.skip_check("Differential Variability", "All available numeric baseline variables were removed because fewer than two distinct non-missing values were present.")))
    else if(!has_comparison_groups)
      check_tables <- c(check_tables, list(.skip_check("Differential Variability", "Numeric baseline variables and an intervention variable with at least two non-missing levels are required.")))
    else
      check_tables <- c(check_tables, list(.skip_check("Differential Variability", "Numeric baseline variables and an intervention variable are required.")))
  }
  
  # Item 9
  if(length(correlated_pairs) > 0 && length(correlated_columns) > 0)
  {
    results <- .unexpectedly_uncorrelated(dataset, correlated_pairs, alpha)
    check_tables <- c(check_tables, list(results[["check_table"]]))
    images <- c(images, results[["images"]])
  } else {
    check_tables <- c(check_tables, list(.skip_check("Unexpectedly Uncorrelated", "No complete correlated variable pairs were provided.")))
  }
  
  # Items 10 and 13
  check_tables <- c(check_tables, list(.implausible_values(dataset, participant_id, info$unexpected, info$enrollment)))
  detail_tables[["4.1"]] <- .detail_randomisation_date_range(dataset, info$enrollment)
  check_tables <- c(check_tables, list(.skip_check("Logical Date Order", "This item needs to be checked mannually. Study-specific repeated visit or event-date variables are required.")))
  
  # Item 11 a and b
  if(has_intervention && intervention_levels_n > 2)
  {
    check_tables <- c(check_tables, list(.skip_check("Allocation Pattern", "This item is currently only implemented for datasets with exactly two intervention levels.")))
  } else if(has_intervention && intervention_levels_n < 2) {
    check_tables <- c(check_tables, list(.skip_check("Allocation Pattern", "An intervention variable with exactly two non-missing levels is required.")))
  } else if(has_intervention)
  {
    dataset_subset <- dataset[, intervention, drop = FALSE]
    allocation_pattern_note <- NULL
    if(has_randomisation)
    {
      order_index <- order(dataset[[info$enrollment$randomisation]], seq_len(nrow(dataset)), na.last = TRUE)
      dataset_subset <- dataset_subset[order_index, , drop = FALSE]
      allocation_pattern_note <- "Allocation order was evaluated after sorting by randomisation date."
    } else {
      allocation_pattern_note <- "Warning: randomisation order was evaluated using row number because no randomisation date was nominated, and this may not be valid."
    }
    check_result <- .excessivelly_homogenous_adjacent(dataset_subset, "intervention", alpha)
    check_result <- .append_allocation_pattern_note(check_result, allocation_pattern_note)
    check_tables <- c(check_tables, list(check_result))
    detail_tables[["5.2"]] <- .detail_allocation_pattern_tests(dataset_subset, alpha)
    detail_tables[["5.2"]][["OrderBasis"]] <- if(has_randomisation) "Sorted by randomisation date" else "Dataset row order; may not be valid"
  } else {
    check_tables <- c(check_tables, list(.skip_check("Allocation Pattern", "An intervention variable is required.")))
  }
  
  if(!has_randomisation)
  {
    check_tables <- c(check_tables, list(.skip_check("Cumulative Allocation", randomisation_missing_message)))
  } else if(has_intervention)
  {
    plot_data <- dataset
    colnames(plot_data)[match(intervention, colnames(plot_data))] <- "intervention"
    colnames(plot_data)[match(info$enrollment$randomisation, colnames(plot_data))] <- "randomisation"
    images <- c(images, list(`Cumulative Allocation` = ggplot2::ggplot(plot_data, ggplot2::aes(.data$randomisation, colour = .data$intervention)) + ggplot2::stat_ecdf() + ggplot2::labs(y = "Empirical Cumulative Distribution")))
    check_tables <- c(check_tables, list(.check_result("Cumulative Allocation", "Displayed", "Cumulative allocation plot generated.")))
  } else {
    check_tables <- c(check_tables, list(.skip_check("Cumulative Allocation", "An intervention variable and randomisation date are required.")))
  }
  
  # Item 12
  if(!has_randomisation)
  {
    check_tables <- c(check_tables, list(.skip_check("Allocation", randomisation_missing_message)))
  } else if(has_comparison_groups)
  {
    results <- .imbalance_day_intervention(dataset, intervention, info$enrollment$randomisation, info$unexpected, alpha)
    check_tables <- c(check_tables, list(results[["check_table"]]))
    images <- c(images, list(Days = results[["image"]]))
    detail_tables[["5.3"]] <- results[["detail_table"]]
  } else {
    check_tables <- c(check_tables, list(.skip_check("Allocation", "An intervention variable with at least two non-missing levels and a randomisation date are required.")))
  }
  
  # Item 14
  summary_table <- NULL
  if(has_intervention)
  {
    summary_columns_available <- summary_columns[summary_columns %in% colnames(dataset)]
    if(length(setdiff(summary_columns_available, intervention)) > 0)
    {
      summary_table <- .external_consistency(dataset[, summary_columns_available, drop = FALSE], intervention)
      check_tables <- c(check_tables, list(.check_result("External Consistency", "Displayed", "Clinical summary table generated for comparison with publications or reports.")))
    } else {
      check_tables <- c(check_tables, list(.skip_check("External Consistency", "No baseline or outcome variables were available for the clinical summary table.")))
    }
  }
  else
    check_tables <- c(check_tables, list(.skip_check("External Consistency", "An intervention variable is required for the clinical summary table.")))
  
  # Item 15
  if(has_comparison_groups)
  {
    outcome_missing_columns <- outcome_columns[outcome_columns %in% colnames(dataset)]
    if(length(outcome_missing_columns) > 0)
    {
      detail_tables[["8.1"]] <- .detail_missing_values_by_intervention(dataset, intervention, outcome_missing_columns, alpha)
      check_tables <- c(check_tables, list(.missing_values_by_intervention_summary(detail_tables[["8.1"]])))
    } else {
      check_tables <- c(check_tables, list(.skip_check("Missing Values by Intervention", "No outcome variables were available for missingness tabulation.")))
    }
  } else {
    check_tables <- c(check_tables, list(.skip_check("Missing Values by Intervention", "An intervention variable with at least two non-missing levels is required.")))
  }
  
  if(has_comparison_groups)
  {
    if(length(dichotomous_event_check[["kept"]]) > 0)
    {
      detail_tables[["8.2"]] <- .detail_events_by_intervention(dataset, intervention, dichotomous_event_check[["kept"]])
      check_result <- .check_result("Implausible Event Rates", "Displayed", "Events and totals table generated for dichotomous baseline and outcome variables by intervention.")
      check_result <- .append_removed_variable_message(check_result, dichotomous_event_check[["removed"]])
      check_tables <- c(check_tables, list(check_result))
    } else if(length(dichotomous_event_check[["removed"]]) > 0) {
      check_tables <- c(check_tables, list(.skip_check("Implausible Event Rates", "All nominated dichotomous baseline or outcome variables were removed because only one non-missing level was present.")))
    } else {
      check_tables <- c(check_tables, list(.skip_check("Implausible Event Rates", "No dichotomous baseline or outcome variables were available for tabulation.")))
    }
  } else {
    check_tables <- c(check_tables, list(.skip_check("Implausible Event Rates", "An intervention variable with at least two non-missing levels is required.")))
  }
  
  check_table_all <- do.call(rbind, check_tables)
  check_table_all <- check_table_all[order(check_table_all$DomainNumber, as.numeric(sub(".*\\.", "", check_table_all$ItemNumber)), check_table_all$Status), ]
  list(check_table = check_table_all, detail_tables = detail_tables, images = images, summary_table = summary_table)
}
