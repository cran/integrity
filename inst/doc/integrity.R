## -----------------------------------------------------------------------------
library(readxl)
examplePath <- system.file("extdata", "dataset.xlsx", package = "integrity")
dataset <- read_excel(examplePath)
dataset[1:5, ]

## -----------------------------------------------------------------------------
library(yaml)
example_path <- system.file("extdata", "variables.yaml", package = "integrity")
dataset_info <- read_yaml(example_path)

## -----------------------------------------------------------------------------
library(integrity)
result <- run_checks(dataset, dataset_info)
names(result)

## -----------------------------------------------------------------------------
head(result[["check_table"]])

## -----------------------------------------------------------------------------
names(result[["images"]])
result[["images"]][["timeAndSize"]]

## -----------------------------------------------------------------------------
result[["summary_table"]]

## -----------------------------------------------------------------------------
sessionInfo()

