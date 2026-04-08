# Data set subset to baseline variables by runner function.
.repeating_baseline <- function(dataset_subset, type = c("across", "within", "across_rare"))
{
  type <- match.arg(type)
  if(type %in% c("across", "across_rare"))
  {
    duplicates <- dataset_subset |>  janitor::get_dupes(dplyr::everything()) |> dplyr::distinct()
  } else if (type == "within"){
    message("Repeating pattern within each baseline algorithm in development")
    return(NULL)
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
    data.frame(Domain = "Unusual or Repeated Patterns", Item = item_text, Status = "Fail", Details = info_text)
  } else {
    data.frame(Domain = "Unusual or Repeated Patterns", Item = item_text, Status = "Pass", Details = "No duplicates found.")
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
.excessivelly_homogenous_adjacent <- function(dataset_subset, type = c("baseline", "intervention"), alpha)
{
  type <- match.arg(type)
  p_values <- apply(dataset_subset, 2, function(variable)
  {
    level <- variable[2:length(variable)]
    preceding_level <- variable[1:(length(variable) - 1)]
    observed_consecutive_pairs <- sum(level == preceding_level)
    observed_consecutive_not_pairs <- nrow(dataset_subset) - observed_consecutive_pairs
    event_probability <- sum(variable == levels(variable)[2])
    expected_consecutive_pairs <- round((nrow(dataset_subset) - 1) * event_probability^2)
    expected_consecutive_not_pairs <- nrow(dataset_subset) - observed_consecutive_not_pairs
    counts <- matrix(c(observed_consecutive_pairs, observed_consecutive_not_pairs,
                       expected_consecutive_pairs, expected_consecutive_not_pairs), ncol = 2)
    chisq.test(counts)$p.value
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
      data.frame(Domain = "Unusual or Repeated Patterns", Item = "Consecutive Baseline Binary", Status = "Fail", Details = info_text)
      } else {
      data.frame(Domain = "Unusual or Repeated Patterns", Item = "Consecutive Baseline Binary", Status = "Pass", Details = "No significant differences using \u03c7\u00b2 test.")
    }
  } else {
    if(any(p_values < alpha, na.rm = TRUE))
    {
      data.frame(Domain = "Randomisation", Item = "Allocation Pattern", Status = "Fail", Details = paste("Intervention", colnames(dataset_subset), "has statistically significant runs of values using \u03c7\u00b2 test."))
    } else {data.frame(Domain = "Randomisation", Item = "Allocation Pattern", Status = "Pass", Details = paste("Intervention", colnames(dataset_subset), "has no statistically significant runs of values using \u03c7\u00b2 test."))}
  }
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
    data.frame(Domain = "Baseline Characteristics", Item = "Excessive Imbalances (Numeric)", Status = "Fail", Details = info_text)
  } else {
    data.frame(Domain = "Baseline Characteristics", Item = "Excessive Imbalances (Numeric)", Status = "Pass", Details = "No significant differences between groups.")
  }
}

# Data set subset to numerical variables with two levels by runner function.
.differential_variability <- function(dataset_subset, intervention, alpha)
{
  variables_check <- setdiff(colnames(dataset_subset), intervention)
  intervention <- dataset_subset[[intervention]]
  p_values <- sapply(dataset_subset[, variables_check], function(variable) car::leveneTest(variable ~ intervention)[["Pr(>F)"]][[1]])
  
  if(any(p_values < alpha, na.rm = TRUE))
  {
    diff_var <- colnames(dataset_subset)[p_values < alpha]
    if(length(diff_var) == 1) info_text <- "Variable" else info_text <- "Variables"
    info_text <- paste(info_text, paste(diff_var, collapse = ", "))
    if(length(diff_var) == 1) info_text <- paste(info_text, "has") else info_text <- paste(info_text, "have")
    info_text <- paste(info_text, "statistically significant differential variability of values using Levene test.")
    data.frame(Domain = "Baseline Characteristics", Item = "Differential Variability", Status = "Fail", Details = info_text)
  } else {
    data.frame(Domain = "Baseline Characteristics", Item = "Differential Variability", Status = "Pass", Details = "No significant differences using Levene test.")
  }
}

# Data set subset to numerical variables with two levels by runner function.
.unexpectedly_uncorrelated <- function(dataset_subset, pairs, alpha)
{
  p_values <- sapply(pairs, function(pair) cor.test(dataset_subset[[pair[1]]], dataset_subset[[pair[2]]])[["p.value"]])
  
  if(any(p_values > alpha, na.rm = TRUE)) # Check greater than because looking for unassociated variables.
  {
    pairs_text <- unname(sapply(pairs[which(p_values > alpha)], function(pair) paste(pair, collapse = ", ")))
    checks_table <- data.frame(Domain = "Correlations", Item = "Unexpectedly Uncorrelated", Status = "Fail", Details = pairs_text)
  } else {
    checks_table <- data.frame(Domain = "Correlations", Item = "Unexpectedly Uncorrelated", Status = "Pass", Details = "No expected pairs of correlated variables are uncorrelated.")
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
  if(!missing(unexpected))
  {
    check_days <- FALSE
    if("days" %in% names(unexpected)) # Take it out of unexpected list for later.
    {
      check_days <- TRUE
      days_unexpected <- unexpected[["days"]][["names"]]
      locale <- unexpected[["days"]][["locale"]]
      unexpected <- unexpected[-match("days", names(unexpected))]
    }
    
    results <- list()
    # Evaluate variables not needing conversion into days.
    results <- mapply(function(values, variable)
    {
      lapply(values, function(value)
      {
        if(grepl("less than", value))
        {
          limit <- as.numeric(gsub("less than ", '', value))
          implausible <- dataset[[variable]] < limit
          if(any(implausible, na.rm = TRUE))
            data.frame(Domain = "Internal Inconsistency", Item = "Implausible Values", Status = "Fail", Details = paste(dataset[[participantID]][implausible], collapse = ", "))
          else
            data.frame(Domain = "Internal Inconsistency", Item = "Implausible Values", Status = "Pass", Details = paste("No values of", variable, value))
        } else if(grepl("greater than", value))
        {
          limit <- as.numeric(gsub("greater than ", '', value))
          implausible <- dataset[[variable]] > limit
          if(any(implausible, na.rm = TRUE))
            data.frame(Domain = "Internal Inconsistency", Item = "Implausible Values", Status = "Fail", Details = paste(dataset[[participantID]][implausible], collapse = ", "))
          else
            data.frame(Domain = "Internal Inconsistency", Item = "Implausible Values", Status = "Pass", Details = paste("No values of", variable, value))
        } else { # equality
          implausible <- dataset[, variable] == value
          if(any(implausible, na.rm = TRUE))
            data.frame(Domain = "Internal Inconsistency", Item = "Implausible Values", Status = "Fail", Details = paste(dataset[[participantID]][implausible], collapse = ", "))
          else
            data.frame(Domain = "Internal Inconsistency", Item = "Implausible Values", Status = "Pass", Details = paste("No values of", variable, "equal to", value))
        }
      })
    }, unexpected, names(unexpected), SIMPLIFY = FALSE)
    results <- unlist(results, recursive = FALSE)
  }
  
  # Enrollment dates and randomisation dates.
  infeasible_start <-  dataset[[enrollment$randomisation]] < dataset[[enrollment$start]]
  infeasible_end <- dataset[[enrollment$randomisation]] > dataset[[enrollment$end]]
  infeasible_date <- infeasible_start | infeasible_end
  if(any(infeasible_date, na.rm = TRUE))
    results <- c(results, list(data.frame(Domain = "Date Violations", Item = "Implausible Randomisation Date", Status = "Fail", Details = paste("Participants", paste(dataset[[participantID]][infeasible_date], collapse = ", ")))))
  else
    results <- c(results, list(data.frame(Domain = "Date Violations", Item = "Implausible Randomisation Date", Status = "Pass", Details = "All randomisation dates are within study start and end date.")))
  
  # Days of the week that the clinic is not operational.
  if(check_days)
  {
    days_checked <- mapply(function(date, variable)
    {
      days <- lubridate::wday(dataset[[date]], label = TRUE, abbr = FALSE, locale = locale)
      is_unexpected <- days %in% days_unexpected
      if(any(is_unexpected, na.rm = TRUE))
      {
        if(sum(is_unexpected) == nrow(dataset)) details <- paste("All participants", variable, "on", paste(unique(days[is_unexpected]), collapse = " or ")) else details <- paste(dataset[[participantID]][is_unexpected], variable, "on", days[is_unexpected])
        data.frame(Domain = "Internal Inconsistency", Item = "Implausible Day", Status = "Fail", Details = details)
      } else {
        data.frame(Domain = "Internal Inconsistency", Item = "Implausible Day", Status = "Pass", Details = paste("No", variable, "on", paste(days_unexpected, collapse = ", ")))
      }
    }, enrollment, names(enrollment), SIMPLIFY = FALSE)
    results <- c(results, days_checked)
  }
  
  results <- do.call(rbind, results)
  rownames(results) <- NULL
  results
}

.imbalance_day_intervention <- function(dataset, intervention, intervention_date, unexpected, alpha)
{
  days_consider <- lubridate::wday(lubridate::today() + 0:6, label = TRUE, abbr = FALSE, locale = unexpected[["days"]][["locale"]])
  if(!missing(unexpected) && "days" %in% names(unexpected))
    days_consider <- setdiff(days_consider, unexpected[["days"]][["names"]])
  
  days_intervention <- lubridate::wday(dataset[[intervention_date]], label = TRUE, abbr = FALSE, locale = unexpected[["days"]][["locale"]])
  interventions_per_day <- table(dataset[[intervention]], days_intervention)
  interventions_per_day <- interventions_per_day[, days_consider]
  p_value <- chisq.test(interventions_per_day)[["p.value"]]
  
  if(p_value < alpha)
  {
    check_table <- data.frame(Domain = "Randomisation", Item = "Allocation", Status = "Fail", Details = "Significant difference of allocations on days using \u03c7\u00b2 test.")
  } else {
    check_table <- data.frame(Domain = "Randomisation", Item = "Allocation", Status = "Pass", Details = "No significant difference of allocations on days using \u03c7\u00b2 test.")
  }
  
  interventions_per_day <- as.data.frame(interventions_per_day)
  image <- ggplot2::ggplot(interventions_per_day, ggplot2::aes(days_intervention, .data$Freq, fill = .data$Var1)) + ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(x = "Day of Week", y = "Number of Participants Randomised", fill = "Intervention")
  list(check_table = check_table, image = image)
}

# Numeric and categorical variables summary table to visually peruse.
.external_consistency <- function(dataset, intervention, p = FALSE, alpha)
{
  summary <- gtsummary::tbl_summary(dataset, by = intervention,
                 statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})",
                                  gtsummary::all_categorical() ~ "{n} ({p}%)"))
  if(p)
  {
    summary <- gtsummary::add_p(summary)
    p_values <-  summary$table_body$p.value
    if(any(p_values < alpha, na.rm = TRUE))
    {
      variables_difer_missing <- summary$table_body[["variable"]][p_values < alpha]
      check_table <- data.frame(Domain = "Plausibility of Data", Item = "Missing Values by Intervention", Status = "Fail", Details = paste("Significant difference of missingness for", paste(variables_difer_missing, ", "), "using \u03c7\u00b2 test."))
    } else {
      check_table <- data.frame(Domain = "Plausibility of Data", Item = "Missing Values by Intervention", Status = "Pass", Details = "No significant difference of missing values between allocations using \u03c7\u00b2 test.")
    }
  } else{summary}
}