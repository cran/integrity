# dataset: data frame of clinical data.
# info: list annotating columns imported from TAML file.
# Checks for presence of columns and makes categorical ones categorical, if not already.
.prepare_data <- function(dataset, info)
{
  missing_columns_message <- function(section, columns)
  {
    columns <- unique(columns[!is.na(columns) & nzchar(columns)])
    if(length(columns) == 0) return(NULL)
    if(length(columns) == 1)
      paste0(section, " variable `", columns, "` does not exist in the dataset. Check spelling or capitalisation.")
    else
      paste0(section, " variables `", paste(columns, collapse = "`, `"), "` do not exist in the dataset. Check spelling or capitalisation.")
  }
  
  # Check the presence of all columns.
  if(is.na(match(info[["participantID"]], colnames(dataset))))
    stop("Participant ID variable `", info[["participantID"]], "` does not exist in the dataset. Check spelling or capitalisation.")
  
  enrollment_columns <- unlist(info[["enrollment"]], use.names = FALSE)
  missing_enrollment <- setdiff(enrollment_columns, colnames(dataset))
  if(length(missing_enrollment) > 0)
    warning(missing_columns_message("Enrollment", missing_enrollment), call. = FALSE)
  
  all_baseline <- unlist(info[["baseline"]], use.names = FALSE)
  missing_baseline <- setdiff(all_baseline, colnames(dataset))
  if(length(missing_baseline) > 0)
    warning(missing_columns_message("Baseline", missing_baseline), call. = FALSE)
  
  intervention_column <- info[["intervention"]]
  if(!is.null(intervention_column) && is.na(match(intervention_column, colnames(dataset))))
    stop(missing_columns_message("Intervention", intervention_column))
  
  all_outcome <- unlist(info[["outcome"]], use.names = FALSE)
  missing_outcome <- setdiff(all_outcome, colnames(dataset))
  if(length(missing_outcome) > 0)
    warning(missing_columns_message("Outcome", missing_outcome), call. = FALSE)
  
  correlated_columns <- unlist(info[["correlated"]], use.names = FALSE)
  missing_correlated <- setdiff(correlated_columns, colnames(dataset))
  if(length(missing_correlated) > 0)
    warning(missing_columns_message("Correlated", missing_correlated), call. = FALSE)
  
  unexpected_columns <- setdiff(names(info[["unexpected"]]), "days")
  missing_unexpected <- setdiff(unexpected_columns, colnames(dataset))
  if(length(missing_unexpected) > 0)
    warning(missing_columns_message("Unexpected-value", missing_unexpected), call. = FALSE)
  
  # Attempt to convert any categorical columns into factors if not already.
  factor_columns <- intervention_column
  if(any(c("dichotomous", "polytomous") %in% names(info[["baseline"]])))
  {
    factor_columns <- c(factor_columns, unlist(info[["baseline"]][c("dichotomous", "polytomous")], use.names = FALSE))
  }
  if(any(c("dichotomous", "polytomous") %in% names(info[["outcome"]][["common"]])))
  {
    factor_columns <- c(factor_columns, unlist(info[["outcome"]][["common"]][c("dichotomous", "polytomous")], use.names = FALSE))
  }
  if(any(c("dichotomous", "polytomous") %in% names(info[["outcome"]][["rare"]])))
  {
    factor_columns <- c(factor_columns, unlist(info[["outcome"]][["rare"]][c("dichotomous", "polytomous")], use.names = FALSE))
  }
  factor_columns <- factor_columns[factor_columns %in% colnames(dataset)]
  
  for(factor_column in factor_columns)
  {
    if(length(unique(dataset[[factor_column]])) > 0.5 * nrow(dataset))
      stop(factor_column, " consists of mostly distinct values and is unlikely to be categorical. Please check the metadata specification for correctness.")

    if(!is.factor(dataset[[factor_column]]))
      dataset[[factor_column]] <- factor(dataset[[factor_column]])
  }
  
  remove <- sapply(dataset, function(column) all(is.na(column)))
  if(any(remove)) dataset <- dataset[, !remove]
    
  dataset
}
