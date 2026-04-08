\name{run_checks}
\alias{run_checks}
\title{Run a Suite of Integirity Checks Based on Dataset Annotation}
\usage{
run_checks(dataset, info, alpha = 0.05)
}
\arguments{
\item{dataset}{A \code{data.frame} of clinical trial data.}
\item{info}{A named list of column names corresponding to different aspects of the clinical trial.
See the vignette for detailed requirements.}
\item{alpha}{Default: 0.05. For checks which use a statistical test, the p-value threshold at which to report a failure.}
}
\description{
Depending on the characteristics of the variables, some test may be skipped if the data type required for the test is not present.
}
\value{
A \code{list} of length 3 with the element named \code{"check_table"} having the table of passes and fails,
the element named \code{"images"} storing ggplot2 plots and the element named \code{"summary_table"} having an overview table of the
variables split by intervention.
}
\examples{
if(interactive())
{
  library(readxl)
  examplePath <- system.file("extdata", "dataset.xlsx", package = "integrity")
  dataset <- read_excel(examplePath)
  library(yaml)
  example_path <- system.file("extdata", "variables.yaml", package = "integrity")
  dataset_info <- read_yaml(example_path)
  result <- run_checks(dataset, dataset_info)
  names(result)
}
}