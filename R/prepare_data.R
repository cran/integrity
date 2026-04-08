# dataset: data frame of clinical data.
# info: list annotating columns imported from TAML file.
# Checks for presence of columns and makes categorical ones categorical, if not already.
.prepare_data <- function(dataset, info)
{
  # Check the presence of all columns.
  if(is.na(match(info[["participantID"]], colnames(dataset))))
    stop("Participant ID ", info[["participantID"]], " not found as column name in data set.")
  
  all_baseline <- unlist(info[["baseline"]])
  baselines <- match(all_baseline, colnames(dataset))
  if(any(is.na(baselines)))
  {
    name_text <- if(sum(is.na(baselines)) > 1) name_text <- "names" else name_text <- "name"
    var_text <- if(sum(is.na(baselines)) > 1) name_text <- "variables" else name_text <- "variable"
    stop("Baseline ", var_text, " ", paste(all_baseline[is.na(baselines)], collapse = ", "), " not found as column ", name_text, " in data set.")
  }
  
  intervention_column <- info[["intervention"]]
  if(is.na(match(intervention_column, colnames(dataset))))
    stop("Intervention ", intervention_column, " not found as column name in data set.")
  
  all_outcome <- unlist(info[["outcome"]])
  outcomes <- match(all_outcome, colnames(dataset))
  if(any(is.na(outcomes)))
  {
    name_text <- if(sum(is.na(outcomes)) > 1) name_text <- "names" else name_text <- "name"
    var_text <- if(sum(is.na(outcomes)) > 1) name_text <- "variables" else name_text <- "variable"
    stop("Outcome ", var_text, " ", paste(all_outcome[is.na(outcomes)], collapse = ", "), " not found as column ", name_text, " in data set.")
  }
  
  # Attempt to convert any categorical columns into factors if not already.
  factor_columns <- intervention_column
  if(any(c("dichotomous", "polytomous") %in% names(info[["baseline"]])))
  {
    factor_columns <- c(factor_columns, unlist(info[["baseline"]][c("dichotomous", "polytomous")]))
  }
  if(any(c("dichotomous", "polytomous") %in% names(info[["outcome"]][["common"]])))
  {
    factor_columns <- c(factor_columns, unlist(info[["outcome"]][["common"]][c("dichotomous", "polytomous")]))
  }
  if(any(c("dichotomous", "polytomous") %in% names(info[["outcome"]][["rare"]])))
  {
    factor_columns <- c(factor_columns, unlist(info[["outcome"]][["rare"]][c("dichotomous", "polytomous")]))
  }
  
  for(factor_column in factor_columns)
  {
    if(length(unique(dataset[[factor_column]])) > 0.5 * nrow(dataset))
      stop(factor_column, " consists of mostly distinct values and is unlikely to be categorical. Please check the YAML file specifiation for correctness.")

    if(!is.factor(dataset[[factor_column]]))
      dataset[[factor_column]] <- factor(dataset[[factor_column]])
  }
  
  remove <- sapply(dataset, function(column) all(is.na(column)))
  if(any(remove)) dataset <- dataset[, !remove]
    
  dataset
}