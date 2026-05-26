## -----------------------------------------------------------------------------
if(requireNamespace("pkgload", quietly = TRUE) && file.exists("../DESCRIPTION")) {
  pkgload::load_all("..")
} else {
  library(integrity)
}

## ----warning =F---------------------------------------------------------------

library(readxl)
examplePath <- system.file("extdata", "dataset.xlsx", package = "integrity")
dataset <- read_excel(examplePath, sheet=1)
dataset[1:5, ]

## -----------------------------------------------------------------------------
dataset_info <- list(
  participantID = "infant_ID",
  enrollment = list(
    start = "enrol_start",
    randomisation = "rand_date",
    end = "enrol_end"
  ),
  baseline = list(
    dichotomous = c("sex"), # add more variables if needed e.g., c("sex", "respiratory_support")
    #polytomous = c("ordinal_or_nominal_variable"), # no polytomous baseline variables in this data set so it's commented out 
    numeric = c("mat_age", "GA_weeks", "birthweight")
    # can add polytomous variables if needed
  ),
  intervention = "treatment_cat",
  outcome = list(
    common = list(
      dichotomous = c("IVH", "NEC"),
      polytomous = c("CLD"), # if certain variable types don't exist, just delete the relevant line.
      numeric = c("hospital_days")
    ),
    rare = list(
      dichotomous = c("inf_death") # add more variables if needed e.g., c("inf_death", "severe_IVH")
    )
  ),
  correlated = list(
    timeAndSize = c("GA_weeks", "birthweight")
  ),
  unexpected = list(
    days = list(
      names = c("Saturday", "Sunday"),
      locale = "C"
    ),
    mat_age = c("less than 10", "greater than 50"),
    GA_weeks = c("less than 22", "greater than 37")
  )
)

## ----eval=FALSE---------------------------------------------------------------
# example_excel_path <- system.file("extdata", "variables_template.xlsx", package = "integrity")
# dataset_info <- read_metadata_excel(example_excel_path)

## -----------------------------------------------------------------------------
result <- run_checks(dataset, dataset_info)
names(result)

## -----------------------------------------------------------------------------
item_1_1 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "1.1", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_1_1, row.names = FALSE)

## -----------------------------------------------------------------------------
item_1_2 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "1.2", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_1_2, row.names = FALSE)

## -----------------------------------------------------------------------------
item_1_3 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "1.3", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_1_3, row.names = FALSE)

## -----------------------------------------------------------------------------
item_1_4 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "1.4", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_1_4, row.names = FALSE)
if("Terminal Digits" %in% names(result[["images"]])) {
  result[["images"]][["Terminal Digits"]]
}

## -----------------------------------------------------------------------------
item_2_1 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "2.1", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_2_1, row.names = FALSE)
if(!is.null(result[["detail_tables"]][["2.1"]])) knitr::kable(result[["detail_tables"]][["2.1"]], row.names = FALSE)

## -----------------------------------------------------------------------------
item_2_2 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "2.2", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_2_2, row.names = FALSE)
if(!is.null(result[["detail_tables"]][["2.2"]])) knitr::kable(result[["detail_tables"]][["2.2"]], row.names = FALSE)

## -----------------------------------------------------------------------------
item_2_3 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "2.3", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_2_3, row.names = FALSE)
if(!is.null(result[["detail_tables"]][["2.3"]])) knitr::kable(result[["detail_tables"]][["2.3"]], row.names = FALSE)

## -----------------------------------------------------------------------------
item_2_4 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "2.4", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_2_4, row.names = FALSE)
if(!is.null(result[["detail_tables"]][["2.4"]])) knitr::kable(result[["detail_tables"]][["2.4"]], row.names = FALSE)

## -----------------------------------------------------------------------------
item_3_1 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "3.1", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_3_1, row.names = FALSE)
correlation_plots <- setdiff(names(result[["images"]]), c("Terminal Digits", "Cumulative Allocation", "Days"))
for(plot_name in correlation_plots) {
  print(result[["images"]][[plot_name]])
}

## -----------------------------------------------------------------------------
item_4_1 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "4.1", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_4_1, row.names = FALSE)

item_4_1_dates <- result[["detail_tables"]][["4.1"]]
if(!is.null(item_4_1_dates)) {
  knitr::kable(item_4_1_dates, row.names = FALSE)
}

## -----------------------------------------------------------------------------
item_4_2 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "4.2", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_4_2, row.names = FALSE)

## -----------------------------------------------------------------------------
item_5_1 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "5.1", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_5_1, row.names = FALSE)
if("Cumulative Allocation" %in% names(result[["images"]])) {
  result[["images"]][["Cumulative Allocation"]]
}

## -----------------------------------------------------------------------------
item_5_2 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "5.2", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_5_2, row.names = FALSE)
item_5_2_test <- result[["detail_tables"]][["5.2"]]
if(!is.null(item_5_2_test)) {
  knitr::kable(item_5_2_test, row.names = FALSE)
}

## -----------------------------------------------------------------------------
item_5_3 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "5.3", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_5_3, row.names = FALSE)
item_5_3_test <- result[["detail_tables"]][["5.3"]]
if(!is.null(item_5_3_test)) {
  knitr::kable(item_5_3_test, row.names = FALSE)
}
if("Days" %in% names(result[["images"]])) {
  result[["images"]][["Days"]]
}

## -----------------------------------------------------------------------------
item_6_1 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "6.1", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_6_1, row.names = FALSE)

## -----------------------------------------------------------------------------
item_7_1 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "7.1", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_7_1, row.names = FALSE)
if(!is.null(result[["summary_table"]])) {
  result[["summary_table"]]
}

## -----------------------------------------------------------------------------
item_8_1 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "8.1", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_8_1, row.names = FALSE)
if(!is.null(result[["detail_tables"]][["8.1"]])) knitr::kable(result[["detail_tables"]][["8.1"]], row.names = FALSE)

## -----------------------------------------------------------------------------
item_8_2 <- result[["check_table"]][result[["check_table"]][["ItemNumber"]] == "8.2", c("Item description", "Status", "Details"), drop = FALSE]
knitr::kable(item_8_2, row.names = FALSE)
if(!is.null(result[["detail_tables"]][["8.2"]])) knitr::kable(result[["detail_tables"]][["8.2"]], row.names = FALSE)

## -----------------------------------------------------------------------------
sessionInfo()

