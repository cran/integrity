\name{run_checks}
\alias{run_checks}
\title{Run a Suite of Integrity Checks Based on Dataset Annotation}
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
\details{
Item 2.1 uses a two-sided Wald-Wolfowitz runs test for each nominated binary
baseline variable. Records are sorted by the randomisation date column nominated
in \code{info$enrollment$randomisation}; tied dates retain original row order.
The column must be a \code{Date} or date-time column. Records with missing dates
are excluded from this item with a warning when other dates are available.
If no usable dates are available, the item uses dataset row order and issues a
warning. The ordering used and excluded date count are recorded in the output.
Missing baseline values are omitted
within each variable, joining the remaining observations into a sequence.
Variables without exactly two observed levels cannot be tested.

When either category has 10 or fewer observations, the test uses the exact
conditional distribution given the category counts. Its p-value is twice the
smaller inclusive tail probability, capped at one. Otherwise it uses a normal
approximation without continuity correction. The detailed output reports the
method, observed and expected runs, missing counts, and p-values. The item flags
any tested variable with a p-value below \code{alpha}; no multiplicity correction
is applied.
}
\value{
A \code{list} with the element named \code{"check_table"} having the table of passes and fails,
the element named \code{"detail_tables"} storing additional per-variable results for selected checks,
the element named \code{"images"} storing ggplot2 plots and the element named \code{"summary_table"} having an overview table of the
baseline and outcome variables split by intervention.
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
