\name{.differential_variability}
\alias{.differential_variability}
\title{Check Variability Between Intervention and Control Groups}
\usage{
.differential_variability(dataset_subset, intervention, alpha)
}
\arguments{
\item{dataset_subset}{A \code{data.frame} of clinical trial data subset to only numeric columns.}
\item{intervention}{Column name of intervention indicator.}
\item{alpha}{p-value signficance threshold.}
}
\description{
Internal function documentation for developers. Levene's test for differential variability.
}
\value{
One-row \code{data.frame} with a Pass or Fail indicator.
}
\examples{
library(readxl)
examplePath <- system.file("extdata", "dataset.xlsx", package = "integrity")
dataset <- read_excel(examplePath)
library(yaml)
infoPath <- system.file("extdata", "variables.yaml", package = "integrity")
info <- read_yaml(infoPath)
dataset <- integrity:::.prepare_data(dataset, info)
numeric_columns <- info$baseline$numeric
dataset_subset <- dataset[, c(numeric_columns, info$intervention)]
integrity:::.differential_variability(dataset_subset, info$intervention, 0.05)
}