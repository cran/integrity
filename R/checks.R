.integrity_items <- data.frame(
  DomainNumber = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 4, 4, 5, 5, 5, 6, 7, 8, 8),
  Domain = c(
    rep("Unusual or repeated data patterns", 4),
    rep("Baseline characteristics", 4),
    "Correlations",
    rep("Date violations", 2),
    rep("Patterns of allocation", 3),
    "Internal inconsistencies",
    "External inconsistencies",
    rep("Plausibility of data", 2)
  ),
  ItemNumber = c("1.1", "1.2", "1.3", "1.4", "2.1", "2.2", "2.3", "2.4", "3.1", "4.1", "4.2", "5.1", "5.2", "5.3", "6.1", "7.1", "8.1", "8.2"),
  Item = c(
    "Repeating patterns within baseline variables",
    "Repeating data patterns across baseline variables",
    "Repeating data patterns across baseline variables and rare variables",
    "Bias in the terminal (rightmost) digits",
    "Excessively homogeneous distribution of binary baseline variables",
    "Excessive imbalances between groups in continuous baseline variables",
    "Excessive imbalances in baseline categorical variables between groups",
    "Significant difference in variance of continuous baseline variables between groups",
    "No association between variables known to be highly correlated",
    "Individual enrolment dates do not fit within study start and end dates",
    "Dates (or visits) are not in logical order",
    "Non-random allocation patterns: plot",
    "Non-random allocation patterns: statistical test",
    "Unexpected imbalance in randomisation day of week",
    "Inconsistent or illogical values across variables within individual participants",
    "IPD do not correspond to publications or reports",
    "Too few missing data or missing data are overly similar between groups",
    "Implausible event rates: outcomes and demographics"
  ),
  result_item = c(
    "Repeated Baselines Within Variables",
    "Repeated Baselines",
    "Repeated Baselines in Rare Outcomes",
    "Terminal Digits",
    "Consecutive Baseline Binary",
    "Excessive Imbalances (Numeric)",
    "Excessive Imbalances (Categorical)",
    "Differential Variability",
    "Unexpectedly Uncorrelated",
    "Implausible Randomisation Date",
    "Logical Date Order",
    "Cumulative Allocation",
    "Allocation Pattern",
    "Allocation",
    "Implausible Values",
    "External Consistency",
    "Missing Values by Intervention",
    "Implausible Event Rates"
  ),
  Item_description = c(
    "Repeated Baselines Within Variables",
    "Repeated Baselines",
    "Repeated Baselines in Rare Outcomes",
    "Terminal Digits",
    "Consecutive Baseline Binary",
    "Excessive Imbalances (Numeric)",
    "Excessive Imbalances (Categorical)",
    "Differential Variability",
    "Unexpectedly Uncorrelated",
    "Implausible Randomisation Date",
    "Logical Date Order",
    "Cumulative Allocation",
    "Allocation Pattern",
    "Allocation",
    "Implausible Values",
    "External Consistency",
    "Missing Values by Intervention",
    "Implausible Event Rates"
  ),
  stringsAsFactors = FALSE
)

.check_result <- function(result_item, status, details)
{
  metadata <- .integrity_items[match(result_item, .integrity_items$result_item), ]
  if(any(is.na(metadata$Item_description)))
    stop("Unknown integrity result item: ", result_item)
  if(status == "Fail")
    status <- "Potential integrity issue"
  result <- data.frame(metadata[, c("DomainNumber", "Domain", "ItemNumber", "Item", "Item_description")], Status = status, Details = details, row.names = NULL)
  result[["Item description"]] <- result[["Item_description"]]
  result
}

.skip_check <- function(result_item, details)
{
  .check_result(result_item, "Skipped", details)
}

# Data set subset to baseline variables by runner function.
.repeating_baseline <- function(dataset_subset, type = c("across", "within", "across_rare"))
{
  type <- match.arg(type)
  if(type %in% c("across", "across_rare"))
  {
    duplicates <- suppressMessages(dataset_subset |>  janitor::get_dupes(dplyr::everything()) |> dplyr::distinct())
  } else if (type == "within"){
    return(.skip_check("Repeated Baselines Within Variables", "This step is mannually peformed through visual inspection of the raw data"))
  }
  
  item_text <- "Repeated Baselines"
  if(type == "across_rare") item_text <- paste(item_text, "in Rare Outcomes")
  if(nrow(duplicates) > 0)
  {  
    info_text <- apply(duplicates, 1, function(duplicate_row)
    {
      duplicate_row <- as.vector(duplicate_row)
      info_text <- paste(paste(colnames(dataset_subset), duplicate_row[1:ncol(dataset_subset)], sep = ':'), collapse = ", ")
      info_text <- paste(info_text, "occurs", duplicate_row[length(duplicate_row)], "times.")
    })
    .check_result(item_text, "Fail", info_text)
  } else {
    .check_result(item_text, "Pass", "No duplicates found.")
  }
}

# Data set subset to numeric variables by runner function. Returns an image, not a quality information row.
.terminal_digits <- function(dataset_subset)
{
  plot_data <- apply(dataset_subset, 2, function(variable)
  {
    terminals <- sapply(variable, function(value)
    {
      if(is.na(value)) return(NA)
      raw_string <- as.character(value)
      no_dot_string <- gsub("\\.", "", raw_string)
      last_char <- substr(no_dot_string, nchar(no_dot_string), nchar(no_dot_string))
      as.numeric(last_char)
    })
    summary <- as.data.frame(table(terminals))
  })
  plot_data <- dplyr::bind_rows(plot_data, .id = "variable")
  plot_data$terminals <- factor(plot_data$terminals, levels = 0:9)
  
  ggplot2::ggplot(plot_data, ggplot2::aes(.data$terminals, .data$Freq)) + ggplot2::geom_col() + ggplot2::facet_wrap(ggplot2::vars(.data$variable), drop = FALSE) + 
    ggplot2::labs(x = "Terminal Digit", y = "Frequency")
}

# Data set subset to categorical variables with two levels by runner function
.adjacent_pair_test_binary <- function(variable)
{
  variable <- factor(variable)
  if(length(levels(variable)) < 2)
    return(list(
      total_adjacent_pairs = NA_integer_,
      observed_consecutive_pairs = NA_integer_,
      observed_non_consecutive_pairs = NA_integer_,
      expected_consecutive_pairs = NA_integer_,
      expected_non_consecutive_pairs = NA_integer_,
      p_value = NA_real_
    ))
  
  level <- variable[2:length(variable)]
  preceding_level <- variable[1:(length(variable) - 1)]
  adjacent_complete <- !is.na(level) & !is.na(preceding_level)
  total_adjacent_pairs <- sum(adjacent_complete)
  if(total_adjacent_pairs == 0)
    return(list(
      total_adjacent_pairs = 0L,
      observed_consecutive_pairs = NA_integer_,
      observed_non_consecutive_pairs = NA_integer_,
      expected_consecutive_pairs = NA_integer_,
      expected_non_consecutive_pairs = NA_integer_,
      p_value = NA_real_
    ))
  
  observed_consecutive_pairs <- sum(level[adjacent_complete] == preceding_level[adjacent_complete])
  observed_non_consecutive_pairs <- total_adjacent_pairs - observed_consecutive_pairs
  event_probability <- mean(variable == levels(variable)[2], na.rm = TRUE)
  expected_consecutive_pairs <- round(total_adjacent_pairs * (event_probability^2 + (1 - event_probability)^2))
  expected_non_consecutive_pairs <- total_adjacent_pairs - expected_consecutive_pairs
  counts <- matrix(c(
    observed_consecutive_pairs, observed_non_consecutive_pairs,
    expected_consecutive_pairs, expected_non_consecutive_pairs
  ), ncol = 2)
  
  list(
    total_adjacent_pairs = total_adjacent_pairs,
    observed_consecutive_pairs = observed_consecutive_pairs,
    observed_non_consecutive_pairs = observed_non_consecutive_pairs,
    expected_consecutive_pairs = expected_consecutive_pairs,
    expected_non_consecutive_pairs = expected_non_consecutive_pairs,
    p_value = stats::chisq.test(counts)$p.value
  )
}

.runs_test_binary <- function(variable)
{
  variable <- stats::na.omit(variable)
  variable <- factor(variable)
  if(length(variable) < 2 || length(levels(variable)) != 2)
    return(list(
      runs = NA_integer_,
      expected_runs = NA_real_,
      variance = NA_real_,
      z = NA_real_,
      p_value = NA_real_,
      note = "Runs test requires exactly two non-missing intervention levels."
    ))
  
  values <- as.character(variable)
  runs <- 1L + sum(values[-1] != values[-length(values)])
  n1 <- sum(variable == levels(variable)[1])
  n2 <- sum(variable == levels(variable)[2])
  expected_runs <- 1 + (2 * n1 * n2) / (n1 + n2)
  variance_runs <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) / (((n1 + n2)^2) * (n1 + n2 - 1))
  if(is.na(variance_runs) || variance_runs <= 0)
    return(list(
      runs = runs,
      expected_runs = expected_runs,
      variance = variance_runs,
      z = NA_real_,
      p_value = NA_real_,
      note = "Runs test variance was not positive."
    ))
  
  z_stat <- (runs - expected_runs) / sqrt(variance_runs)
  list(
    runs = runs,
    expected_runs = expected_runs,
    variance = variance_runs,
    z = z_stat,
    p_value = 2 * stats::pnorm(-abs(z_stat)),
    note = NA_character_
  )
}

.excessivelly_homogenous_adjacent <- function(dataset_subset, type = c("baseline", "intervention"), alpha)
{
  type <- match.arg(type)
  p_values <- apply(dataset_subset, 2, function(variable)
  {
    .adjacent_pair_test_binary(variable)$p_value
  })
  if(type == "baseline")
  {
    if(any(p_values < alpha, na.rm = TRUE))
    {
      non_uniform <- colnames(dataset_subset)[p_values < alpha]
      if(length(non_uniform) == 1) info_text <- "Variable" else info_text <- "Variables"
      info_text <- paste(info_text, paste(non_uniform, collapse = ", "))
      if(length(non_uniform) == 1) info_text <- paste(info_text, "has") else info_text <- paste(info_text, "have")
      info_text <- paste(info_text, "statistically significant runs of values using \u03c7\u00b2 test.")
      .check_result("Consecutive Baseline Binary", "Fail", info_text)
      } else {
      .check_result("Consecutive Baseline Binary", "Pass", "No significant differences using \u03c7\u00b2 test.")
    }
  } else {
    intervention_name <- colnames(dataset_subset)[1]
    variable <- dataset_subset[[1]]
    adjacent_result <- .adjacent_pair_test_binary(variable)
    runs_result <- .runs_test_binary(variable)
    is_flagged <- isTRUE(adjacent_result$p_value < alpha) || isTRUE(runs_result$p_value < alpha)
    if(is_flagged)
    {
      .check_result("Allocation Pattern", "Fail", paste("Intervention", intervention_name, "has a statistically significant result using the adjacent-pairs chi-squared test or runs test."))
    } else {
      .check_result("Allocation Pattern", "Pass", paste("Intervention", intervention_name, "has no statistically significant result using the adjacent-pairs chi-squared test or runs test."))
    }
  }
}

.detail_excessivelly_homogenous_adjacent <- function(dataset_subset, alpha)
{
  do.call(rbind, lapply(colnames(dataset_subset), function(variable_name)
  {
    variable <- factor(dataset_subset[[variable_name]])
    if(length(levels(variable)) < 2)
      return(data.frame(
        Variable = variable_name,
        TotalAdjacentPairs = NA_integer_,
        ObservedConsecutivePairs = NA_integer_,
        ObservedNonConsecutivePairs = NA_integer_,
        ExpectedConsecutivePairs = NA_integer_,
        ExpectedNonConsecutivePairs = NA_integer_,
        PValue = NA_real_,
        Significant = NA,
        row.names = NULL
      ))
    
    level <- variable[2:length(variable)]
    preceding_level <- variable[1:(length(variable) - 1)]
    adjacent_complete <- !is.na(level) & !is.na(preceding_level)
    total_adjacent_pairs <- sum(adjacent_complete)
    if(total_adjacent_pairs == 0)
      return(data.frame(
        Variable = variable_name,
        TotalAdjacentPairs = 0L,
        ObservedConsecutivePairs = NA_integer_,
        ObservedNonConsecutivePairs = NA_integer_,
        ExpectedConsecutivePairs = NA_integer_,
        ExpectedNonConsecutivePairs = NA_integer_,
        PValue = NA_real_,
        Significant = NA,
        row.names = NULL
      ))
    
    observed_consecutive_pairs <- sum(level[adjacent_complete] == preceding_level[adjacent_complete])
    observed_non_consecutive_pairs <- total_adjacent_pairs - observed_consecutive_pairs
    event_probability <- mean(variable == levels(variable)[2], na.rm = TRUE)
    expected_consecutive_pairs <- round(total_adjacent_pairs * (event_probability^2 + (1 - event_probability)^2))
    expected_non_consecutive_pairs <- total_adjacent_pairs - expected_consecutive_pairs
    counts <- matrix(c(
      observed_consecutive_pairs, observed_non_consecutive_pairs,
      expected_consecutive_pairs, expected_non_consecutive_pairs
    ), ncol = 2)
    p_value <- stats::chisq.test(counts)$p.value
    
    data.frame(
      Variable = variable_name,
      TotalAdjacentPairs = total_adjacent_pairs,
      ObservedConsecutivePairs = observed_consecutive_pairs,
      ObservedNonConsecutivePairs = observed_non_consecutive_pairs,
      ExpectedConsecutivePairs = expected_consecutive_pairs,
      ExpectedNonConsecutivePairs = expected_non_consecutive_pairs,
      PValue = signif(p_value, 4),
      Significant = p_value < alpha,
      row.names = NULL
    )
  }))
}

.chisq_test_with_sparse_handling <- function(x, B = 10000)
{
  untestable_result <- function(method)
  {
    structure(
      list(
        statistic = NA_real_,
        parameter = NA_real_,
        p.value = NA_real_,
        method = method,
        data.name = deparse(substitute(x))
      ),
      class = "htest"
    )
  }
  
  if(sum(x, na.rm = TRUE) == 0 || any(dim(x) < 2))
    return(list(
      result = untestable_result("Chi-squared test could not be calculated because the table had insufficient non-missing counts"),
      method = "Chi-squared test could not be calculated because the table had insufficient non-missing counts",
      sparse_counts = NA
    ))
  
  standard_result <- tryCatch(
    suppressWarnings(stats::chisq.test(x)),
    error = function(e) untestable_result(paste("Chi-squared test could not be calculated:", conditionMessage(e)))
  )
  
  if(is.na(standard_result[["p.value"]]))
    return(list(
      result = untestable_result("Chi-squared test returned an undefined p-value; this can occur when randomisation-day counts are too sparse or degenerate"),
      method = "Chi-squared test returned an undefined p-value; this can occur when randomisation-day counts are too sparse or degenerate",
      sparse_counts = NA
    ))
  
  expected_counts <- standard_result[["expected"]]
  sparse_counts <- any(expected_counts < 5, na.rm = TRUE)
  
  if(!sparse_counts)
    return(list(
      result = standard_result,
      method = "Pearson's chi-squared test",
      sparse_counts = FALSE
    ))
  
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if(had_seed)
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if(had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  
  set.seed(1)
  simulated_result <- tryCatch(
    suppressWarnings(stats::chisq.test(x, simulate.p.value = TRUE, B = B)),
    error = function(e) untestable_result(paste("Simulated chi-squared test could not be calculated:", conditionMessage(e)))
  )
  list(
    result = simulated_result,
    method = if(is.na(simulated_result[["p.value"]])) simulated_result[["method"]] else paste0("Simulated chi-squared test used because expected counts were sparse (", B, " replicates)"),
    sparse_counts = TRUE
  )
}

.detail_allocation_day_test <- function(dataset, intervention, intervention_date, unexpected, alpha)
{
  locale <- "C"
  if(!missing(unexpected) && !is.null(unexpected) && "days" %in% names(unexpected))
  {
    locale <- unexpected[["days"]][["locale"]]
  }
  
  weekday_levels <- as.character(lubridate::wday(1:7, label = TRUE, abbr = FALSE, locale = locale))
  days_intervention <- factor(
    as.character(lubridate::wday(dataset[[intervention_date]], label = TRUE, abbr = FALSE, locale = locale)),
    levels = weekday_levels,
    ordered = TRUE
  )
  day_counts <- table(days_intervention)
  interventions_per_day <- table(dataset[[intervention]], days_intervention)
  overall_day_result <- .chisq_test_with_sparse_handling(day_counts)
  by_group_result <- .chisq_test_with_sparse_handling(interventions_per_day)
  
  data.frame(
    Test = c(
      "Chi-squared goodness-of-fit test of randomisation day overall",
      "Chi-squared test of randomisation day by intervention group"
    ),
    Method = c(
      overall_day_result$method,
      by_group_result$method
    ),
    Statistic = c(
      signif(unname(overall_day_result$result$statistic), 4),
      signif(unname(by_group_result$result$statistic), 4)
    ),
    DF = c(
      unname(overall_day_result$result$parameter),
      unname(by_group_result$result$parameter)
    ),
    PValue = c(
      signif(overall_day_result$result$p.value, 4),
      signif(by_group_result$result$p.value, 4)
    ),
    Significant = c(
      overall_day_result$result$p.value < alpha,
      by_group_result$result$p.value < alpha
    ),
    row.names = NULL
  )
}

.detail_allocation_pattern_tests <- function(dataset_subset, alpha)
{
  variable_name <- colnames(dataset_subset)[1]
  variable <- dataset_subset[[1]]
  adjacent_result <- .adjacent_pair_test_binary(variable)
  runs_result <- .runs_test_binary(variable)
  
  data.frame(
    Test = c("Adjacent-pairs chi-squared test", "Runs test"),
    Variable = variable_name,
    Statistic = c(NA_real_, signif(runs_result$z, 4)),
    PValue = c(signif(adjacent_result$p_value, 4), signif(runs_result$p_value, 4)),
    Significant = c(adjacent_result$p_value < alpha, runs_result$p_value < alpha),
    Details = c(
      paste0(
        "Observed consecutive pairs: ", adjacent_result$observed_consecutive_pairs,
        "; expected consecutive pairs: ", adjacent_result$expected_consecutive_pairs,
        "; total adjacent pairs: ", adjacent_result$total_adjacent_pairs
      ),
      ifelse(
        is.na(runs_result$note),
        paste0(
          "Observed runs: ", runs_result$runs,
          "; expected runs: ", signif(runs_result$expected_runs, 4)
        ),
        runs_result$note
      )
    ),
    row.names = NULL
  )
}

.append_allocation_pattern_note <- function(check_table, note)
{
  if(is.null(note) || !nzchar(note)) return(check_table)
  check_table[["Details"]] <- paste(check_table[["Details"]], note)
  check_table
}

# Data set subset to numeric variables by runner function.
# Need to know null hypothesis, which ism't always uniformity.
# .terminal_digits <- function(dataset_subset, alpha)
# {
#   
#   p_Values <- apply(dataset_subset, 2, function(variable) terminaldigits::td_uniformity(variable, decimals = 0))
#   if(any(p_values < alpha, na.rm = TRUE))
#   {
#     non_uniform <- colnames(dataset_subset)[p_values < alpha]
#     if(length(non_uniform) == 1) info_text <- "Variable" else info_text <- "Variables"
#     info_text <- paste(info_text, paste(non_uniform, collapse = ", "), "have non-uniformity.")
#     data.frame(Domain = "Unusual or Repeated Patterns", Item = "Terminal Digits", Status = "Fail", Details = info_text)
#   } else {
#     data.frame(Domain = "Unusual or Repeated Patterns", Item = "Terminal Digits", Status = "Pass", Details = "No significant differences using \u03c7\u00b2 test.")
#   }
# }

# Data set subset to numerical variables or categorical with two levels by runner function.
.excessivelly_different <- function(dataset_subset, intervention, alpha, type = c("numeric", "categorical"))
{
  type <- match.arg(type)
  if(type == "numeric")
    summary <- gtsummary::tbl_summary(dataset_subset, by = dplyr::all_of(intervention),
                                      statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"))
  else
    summary <- gtsummary::tbl_summary(dataset_subset, by = dplyr::all_of(intervention))
  summary <- gtsummary::add_p(summary)
  if(any(summary$table_body$p.value < alpha, na.rm = TRUE))
  {
    rows_different <- which(summary$table_body$p.value < alpha)
    if(length(rows_different) == 1) info_text <- "Variable" else info_text <- "Variables"
    info_text <- paste(summary$table_body[["variable"]][rows_different], collapse = ", ")
    if(length(rows_different) == 1) info_text <- paste(info_text, "has") else info_text <- paste(info_text, "have")
    info_text <- paste(info_text, "statistically significant excessive imbalances.")
    .check_result(paste0("Excessive Imbalances (", tools::toTitleCase(type), ")"), "Fail", info_text)
  } else {
    .check_result(paste0("Excessive Imbalances (", tools::toTitleCase(type), ")"), "Pass", "No significant differences between groups.")
  }
}

.detail_excessivelly_different <- function(dataset_subset, intervention, type = c("numeric", "categorical"))
{
  type <- match.arg(type)
  if(type == "numeric")
    summary <- gtsummary::tbl_summary(dataset_subset, by = dplyr::all_of(intervention),
                                      statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"))
  else
    summary <- gtsummary::tbl_summary(dataset_subset, by = dplyr::all_of(intervention))
  summary <- gtsummary::add_p(summary)
  stat_columns <- grep("^stat_", colnames(summary$table_body), value = TRUE)
  detail_table <- summary$table_body[, c("label", stat_columns, "p.value")]
  intervention_levels <- levels(factor(dataset_subset[[intervention]]))
  pretty_levels <- gsub("_", " ", intervention_levels, fixed = TRUE)
  colnames(detail_table) <- c(
    if(type == "numeric") "Variable" else "VariableOrLevel",
    if(type == "numeric") paste0("Group ", pretty_levels, " MeanSD") else paste0("Group ", pretty_levels),
    "PValue"
  )
  detail_table
}

# Data set subset to numerical variables with two levels by runner function.
.differential_variability <- function(dataset_subset, intervention, alpha)
{
  variables_check <- setdiff(colnames(dataset_subset), intervention)
  intervention <- dataset_subset[[intervention]]
  p_values <- sapply(dataset_subset[, variables_check, drop = FALSE], function(variable)
  {
    test_data <- data.frame(variable = variable, intervention = intervention)
    test_data <- test_data[complete.cases(test_data), , drop = FALSE]
    if(length(unique(test_data$intervention)) < 2) return(NA_real_)
    car::leveneTest(variable ~ intervention, data = test_data)[["Pr(>F)"]][[1]]
  })
  
  if(any(p_values < alpha, na.rm = TRUE))
  {
    diff_var <- names(p_values)[p_values < alpha]
    if(length(diff_var) == 1) info_text <- "Variable" else info_text <- "Variables"
    info_text <- paste(info_text, paste(diff_var, collapse = ", "))
    if(length(diff_var) == 1) info_text <- paste(info_text, "has") else info_text <- paste(info_text, "have")
    info_text <- paste(info_text, "statistically significant differential variability of values using Levene test.")
    .check_result("Differential Variability", "Fail", info_text)
  } else {
    .check_result("Differential Variability", "Pass", "No significant differences using Levene test.")
  }
}

.detail_differential_variability <- function(dataset_subset, intervention, alpha)
{
  variables_check <- setdiff(colnames(dataset_subset), intervention)
  intervention_values <- dataset_subset[[intervention]]
  do.call(rbind, lapply(variables_check, function(variable_name)
  {
    test_data <- data.frame(
      variable = dataset_subset[[variable_name]],
      intervention = intervention_values
    )
    test_data <- test_data[stats::complete.cases(test_data), , drop = FALSE]
    if(length(unique(test_data$intervention)) < 2)
      return(data.frame(
        Variable = variable_name,
        DF1 = NA_real_,
        DF2 = NA_real_,
        FStatistic = NA_real_,
        PValue = NA_real_,
        Significant = NA,
        row.names = NULL
      ))
    
    levene_result <- car::leveneTest(variable ~ intervention, data = test_data)
    data.frame(
      Variable = variable_name,
      DF1 = levene_result[["Df"]][1],
      DF2 = levene_result[["Df"]][2],
      FStatistic = signif(levene_result[["F value"]][1], 4),
      PValue = signif(levene_result[["Pr(>F)"]][1], 4),
      Significant = levene_result[["Pr(>F)"]][1] < alpha,
      row.names = NULL
    )
  }))
}

.detail_randomisation_date_range <- function(dataset, enrollment)
{
  enrollment_columns <- c(enrollment$start, enrollment$randomisation, enrollment$end)
  if(!.has_columns(dataset, enrollment_columns) || !all(sapply(enrollment_columns, function(column) .is_date_column(dataset, column))))
    return(NULL)
  
  start_dates <- dataset[[enrollment$start]]
  randomisation_dates <- dataset[[enrollment$randomisation]]
  end_dates <- dataset[[enrollment$end]]
  safe_min <- function(x) if(all(is.na(x))) NA else min(x, na.rm = TRUE)
  safe_max <- function(x) if(all(is.na(x))) NA else max(x, na.rm = TRUE)
  
  data.frame(
    `Study Start Date` = as.character(safe_min(start_dates)),
    `Minimum Randomisation Date` = as.character(safe_min(randomisation_dates)),
    `Study End Date` = as.character(safe_max(end_dates)),
    `Maximum Randomisation Date` = as.character(safe_max(randomisation_dates)),
    row.names = NULL,
    check.names = FALSE
  )
}

# Data set subset to numerical variables with two levels by runner function.
.unexpectedly_uncorrelated <- function(dataset_subset, pairs, alpha)
{
  pair_results <- lapply(pairs, function(pair)
  {
    x <- dataset_subset[[pair[1]]]
    y <- dataset_subset[[pair[2]]]
    pair_name <- paste(pair, collapse = ", ")
    
    if(!is.numeric(x) || !is.numeric(y))
      return(list(pair = pair, pair_name = pair_name, p_value = NA_real_, note = "both variables must be numeric"))
    
    complete <- stats::complete.cases(x, y) & is.finite(x) & is.finite(y)
    x_complete <- x[complete]
    y_complete <- y[complete]
    
    if(length(x_complete) < 3)
      return(list(pair = pair, pair_name = pair_name, p_value = NA_real_, note = "fewer than 3 complete finite observations"))
    
    if(length(unique(x_complete)) < 2 || length(unique(y_complete)) < 2)
      return(list(pair = pair, pair_name = pair_name, p_value = NA_real_, note = "at least one variable was constant"))
    
    p_value <- tryCatch(
      stats::cor.test(x_complete, y_complete)[["p.value"]],
      error = function(e) NA_real_
    )
    note <- if(is.na(p_value)) "correlation test could not be calculated" else NA_character_
    list(pair = pair, pair_name = pair_name, p_value = p_value, note = note)
  })
  
  p_values <- vapply(pair_results, function(result) result$p_value, numeric(1))
  names(p_values) <- vapply(pair_results, function(result) result$pair_name, character(1))
  untestable <- vapply(pair_results, function(result) !is.na(result$note), logical(1))
  untestable_text <- vapply(pair_results[untestable], function(result) paste0(result$pair_name, " (", result$note, ")"), character(1))
  untestable_note <- if(length(untestable_text) > 0) paste("Untestable pairs:", paste(untestable_text, collapse = "; ")) else NULL
  
  if(all(is.na(p_values)))
  {
    details <- "No complete correlated variable pairs could be tested."
    if(!is.null(untestable_note)) details <- paste(details, untestable_note)
    checks_table <- .skip_check("Unexpectedly Uncorrelated", details)
  } else if(any(p_values > alpha, na.rm = TRUE)) # Check greater than because looking for unassociated variables.
  {
    pairs_text <- names(p_values)[p_values > alpha & !is.na(p_values)]
    details <- paste(pairs_text, collapse = "; ")
    if(!is.null(untestable_note)) details <- paste(details, untestable_note)
    checks_table <- .check_result("Unexpectedly Uncorrelated", "Fail", details)
  } else {
    details <- "No expected pairs of correlated variables are uncorrelated."
    if(!is.null(untestable_note)) details <- paste(details, untestable_note)
    checks_table <- .check_result("Unexpectedly Uncorrelated", "Pass", details)
  }

  scatter_plots <- lapply(pairs, function(pair)
                   {
                     ggpubr::ggscatter(dataset_subset, x = pair[1], y = pair[2], add = "reg.line", conf.int = TRUE,
                                       add.params = list(color = "red"), cor.coef = TRUE, cor.method = "pearson")
                   })
  
  list(check_table = checks_table, images = scatter_plots)
}

.implausible_values <- function(dataset, participantID, unexpected, enrollment)
{
  results <- list()
  check_days <- FALSE
  if(!missing(unexpected) && !is.null(unexpected) && length(unexpected) > 0)
  {
    if("days" %in% names(unexpected)) # Take it out of unexpected list for later.
    {
      check_days <- TRUE
      days_unexpected <- unexpected[["days"]][["names"]]
      locale <- unexpected[["days"]][["locale"]]
      days_variables <- unexpected[["days"]][["variables"]]
      unexpected <- unexpected[-match("days", names(unexpected))]
    }
    
    # Evaluate variables not needing conversion into days.
    missing_unexpected <- setdiff(names(unexpected), colnames(dataset))
    if(length(missing_unexpected) > 0)
    {
      results <- c(results, list(.skip_check("Implausible Values", paste("Unexpected-value variables not found:", paste(missing_unexpected, collapse = ", ")))))
      unexpected <- unexpected[names(unexpected) %in% colnames(dataset)]
    }
    
    if(length(unexpected) > 0)
    {
      results_unexpected <- mapply(function(values, variable)
    {
      lapply(values, function(value)
      {
        if(grepl("less than", value))
        {
          limit <- as.numeric(gsub("less than ", '', value))
          implausible <- dataset[[variable]] < limit
          if(any(implausible, na.rm = TRUE))
            .check_result("Implausible Values", "Fail", paste("Rule failed: values of", variable, "should not be less than", limit, ". Participants:", paste(paste(dataset[[participantID]][implausible], dataset[[variable]][implausible], sep = "-"), collapse = ", ")))
          else
            .check_result("Implausible Values", "Pass", paste("No values of", variable, value))
        } else if(grepl("greater than", value))
        {
          limit <- as.numeric(gsub("greater than ", '', value))
          implausible <- dataset[[variable]] > limit
          if(any(implausible, na.rm = TRUE))
            .check_result("Implausible Values", "Fail", paste("Rule failed: values of", variable, "should not be greater than", limit, ". Participants:", paste(paste(dataset[[participantID]][implausible], dataset[[variable]][implausible], sep = "-"), collapse = ", ")))
          else
            .check_result("Implausible Values", "Pass", paste("No values of", variable, value))
        } else { # equality
          implausible <- dataset[, variable] == value
          if(any(implausible, na.rm = TRUE))
            .check_result("Implausible Values", "Fail", paste("Rule failed: values of", variable, "should not be equal to", value, ". Participants:", paste(paste(dataset[[participantID]][implausible], dataset[[variable]][implausible], sep = "-"), collapse = ", ")))
          else
            .check_result("Implausible Values", "Pass", paste("No values of", variable, "equal to", value))
        }
      })
    }, unexpected, names(unexpected), SIMPLIFY = FALSE)
      results <- c(results, unlist(results_unexpected, recursive = FALSE))
    } else if(length(missing_unexpected) == 0) {
      results <- c(results, list(.skip_check("Implausible Values", "No unexpected values were provided.")))
    }
  } else {
    results <- c(results, list(.skip_check("Implausible Values", "No unexpected values were provided.")))
  }
  
  # Enrollment dates and randomisation dates.
  enrollment_columns <- c(enrollment$start, enrollment$randomisation, enrollment$end)
  if(.has_columns(dataset, enrollment_columns) && all(sapply(enrollment_columns, function(column) .is_date_column(dataset, column))))
  {
    infeasible_start <-  dataset[[enrollment$randomisation]] < dataset[[enrollment$start]]
    infeasible_end <- dataset[[enrollment$randomisation]] > dataset[[enrollment$end]]
    infeasible_date <- infeasible_start | infeasible_end
    if(any(infeasible_date, na.rm = TRUE))
      results <- c(results, list(.check_result("Implausible Randomisation Date", "Fail", paste("Participants", paste(dataset[[participantID]][infeasible_date], collapse = ", ")))))
    else
      results <- c(results, list(.check_result("Implausible Randomisation Date", "Pass", "All randomisation dates are within study start and end date.")))
  } else {
    results <- c(results, list(.skip_check("Implausible Randomisation Date", "Enrollment start, randomisation, and end date columns are required.")))
  }
  
  # Days of the week that the clinic is not operational.
  if(check_days)
  {
    date_enrollment <- enrollment["randomisation"]
    if(!is.null(days_variables))
    {
      date_enrollment <- enrollment[names(enrollment) %in% days_variables]
      extra_date_columns <- days_variables[days_variables %in% colnames(dataset)]
      if(length(extra_date_columns) > 0)
        date_enrollment <- c(date_enrollment, stats::setNames(extra_date_columns, extra_date_columns))
    }
    date_enrollment <- date_enrollment[sapply(date_enrollment, function(date) .is_date_column(dataset, date))]
    if(length(date_enrollment) == 0)
    {
      results <- c(results, list(.skip_check("Implausible Values", "No date columns were available for day-of-week checks.")))
    } else {
      days_checked <- mapply(function(date, variable)
    {
      days <- lubridate::wday(dataset[[date]], label = TRUE, abbr = FALSE, locale = locale)
      is_unexpected <- days %in% days_unexpected
      if(any(is_unexpected, na.rm = TRUE))
      {
        if(sum(is_unexpected, na.rm = TRUE) == sum(!is.na(days))) details <- paste("All non-missing", variable, "dates are on", paste(unique(days[is_unexpected]), collapse = " or ")) else details <- paste(dataset[[participantID]][is_unexpected], variable, "date on", days[is_unexpected])
        .check_result("Implausible Values", "Fail", details)
      } else {
        .check_result("Implausible Values", "Pass", paste("No", variable, "dates are on", paste(days_unexpected, collapse = ", ")))
      }
      }, date_enrollment, names(date_enrollment), SIMPLIFY = FALSE)
      results <- c(results, days_checked)
    }
  }
  
  results <- do.call(rbind, results)
  rownames(results) <- NULL
  results
}

.imbalance_day_intervention <- function(dataset, intervention, intervention_date, unexpected, alpha)
{
  locale <- "C"
  if(!missing(unexpected) && !is.null(unexpected) && "days" %in% names(unexpected))
  {
    locale <- unexpected[["days"]][["locale"]]
  }
  
  days_intervention <- lubridate::wday(dataset[[intervention_date]], label = TRUE, abbr = FALSE, locale = locale)
  weekday_levels <- as.character(lubridate::wday(1:7, label = TRUE, abbr = FALSE, locale = locale))
  days_intervention <- factor(as.character(days_intervention), levels = weekday_levels, ordered = TRUE)
  interventions_per_day <- table(dataset[[intervention]], days_intervention)
  chisq_result <- .chisq_test_with_sparse_handling(interventions_per_day)
  p_value <- chisq_result$result[["p.value"]]
  
  if(is.na(p_value))
  {
    check_table <- .skip_check("Allocation", paste(chisq_result$method, "."))
  } else if(p_value < alpha)
  {
    check_table <- .check_result("Allocation", "Fail", paste("Significant difference of allocations on days using", chisq_result$method, "."))
  } else {
    check_table <- .check_result("Allocation", "Pass", paste("No significant difference of allocations on days using", chisq_result$method, "."))
  }
  
  interventions_per_day <- as.data.frame(interventions_per_day)
  interventions_per_day$days_intervention <- factor(interventions_per_day$days_intervention, levels = weekday_levels, ordered = TRUE)
  image <- ggplot2::ggplot(interventions_per_day, ggplot2::aes(days_intervention, .data$Freq, fill = .data$Var1)) + ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(x = "Day of Week", y = "Number of Participants Randomised", fill = "Intervention")
  list(check_table = check_table, image = image, detail_table = .detail_allocation_day_test(dataset, intervention, intervention_date, unexpected, alpha))
}

# Numeric and categorical variables summary table to visually peruse.
.external_consistency <- function(dataset, intervention, p = FALSE, alpha)
{
  summary <- gtsummary::tbl_summary(dataset, by = dplyr::all_of(intervention),
                 statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})",
                                   gtsummary::all_categorical() ~ "{n} ({p}%)"))
  if(p)
  {
    summary <- gtsummary::add_p(summary)
    p_values <-  summary$table_body$p.value
    if(any(p_values < alpha, na.rm = TRUE))
    {
      variables_difer_missing <- summary$table_body[["variable"]][p_values < alpha]
      check_table <- .check_result("Missing Values by Intervention", "Fail", paste("Significant difference of missingness for", paste(variables_difer_missing, ", "), "using \u03c7\u00b2 test."))
    } else {
      check_table <- .check_result("Missing Values by Intervention", "Pass", "No significant difference of missing values between allocations using \u03c7\u00b2 test.")
    }
  } else{summary}
}

.detail_missing_values_by_intervention <- function(dataset, intervention, outcome_columns, alpha)
{
  outcome_columns <- outcome_columns[outcome_columns %in% colnames(dataset)]
  if(length(outcome_columns) == 0 || !(intervention %in% colnames(dataset)))
    return(NULL)
  
  intervention_values <- factor(dataset[[intervention]])
  intervention_levels <- levels(intervention_values)
  
  do.call(rbind, lapply(outcome_columns, function(variable_name)
  {
    missing_status <- factor(ifelse(is.na(dataset[[variable_name]]), "Missing", "Not Missing"),
                             levels = c("Not Missing", "Missing"))
    counts_table <- table(intervention_values, missing_status)
    totals <- rowSums(counts_table)
    missing_counts <- counts_table[, "Missing"]
    missing_percents <- round(100 * missing_counts / totals, 1)
    
    p_value <- NA_real_
    if(all(dim(counts_table) > 1) && all(rowSums(counts_table) > 0) && all(colSums(counts_table) > 0))
      p_value <- suppressWarnings(stats::chisq.test(counts_table)$p.value)
    
    result_row <- data.frame(Variable = variable_name, row.names = NULL)
    for(level_name in intervention_levels)
    {
      pretty_level_name <- gsub("_", " ", level_name, fixed = TRUE)
      result_row[[paste0("Missing Count ", pretty_level_name)]] <- unname(missing_counts[level_name])
      result_row[[paste0("Total ", pretty_level_name)]] <- unname(totals[level_name])
      result_row[[paste0("Missing Percent ", pretty_level_name)]] <- unname(missing_percents[level_name])
    }
    result_row[["PValue"]] <- signif(p_value, 4)
    result_row[["Significant"]] <- ifelse(is.na(p_value), NA, p_value < alpha)
    result_row
  }))
}

.missing_values_by_intervention_summary <- function(detail_table)
{
  if(is.null(detail_table) || nrow(detail_table) == 0)
    return(.skip_check("Missing Values by Intervention", "No outcome variables were available for missingness tabulation."))
  
  significant_rows <- !is.na(detail_table[["Significant"]]) & detail_table[["Significant"]]
  if(any(significant_rows))
  {
    variables_differ_missing <- detail_table[["Variable"]][significant_rows]
    .check_result(
      "Missing Values by Intervention",
      "Fail",
      paste("Significant difference of missingness for", paste(variables_differ_missing, collapse = ", "), "using \u03c7\u00b2 test.")
    )
  } else {
    .check_result("Missing Values by Intervention", "Pass", "No significant difference of missing values between allocations using \u03c7\u00b2 test.")
  }
}

.detail_events_by_intervention <- function(dataset, intervention, variables)
{
  variables <- variables[variables %in% colnames(dataset)]
  if(length(variables) == 0 || !(intervention %in% colnames(dataset)))
    return(NULL)
  
  intervention_values <- factor(dataset[[intervention]])
  intervention_levels <- levels(intervention_values)
  
  do.call(rbind, lapply(variables, function(variable_name)
  {
    variable <- factor(dataset[[variable_name]])
    if(length(levels(variable)) < 2)
      return(NULL)
    
    event_level <- levels(variable)[2]
    result_row <- data.frame(
      Variable = variable_name,
      EventLevel = event_level,
      row.names = NULL
    )
    for(level_name in intervention_levels)
    {
      group_values <- variable[intervention_values == level_name]
      total_non_missing <- sum(!is.na(group_values))
      event_count <- sum(group_values == event_level, na.rm = TRUE)
      event_percent <- if(total_non_missing > 0) round(100 * event_count / total_non_missing, 1) else NA_real_
      pretty_level_name <- gsub("_", " ", level_name, fixed = TRUE)
      result_row[[paste0("Events ", pretty_level_name)]] <- event_count
      result_row[[paste0("Total ", pretty_level_name)]] <- total_non_missing
      result_row[[paste0("Percent ", pretty_level_name)]] <- event_percent
    }
    result_row
  }))
}
