\name{.imbalance_day_intervention}
\alias{.imbalance_day_intervention}
\title{Check Day of Week of Randomisation for Non-uniformity}
\usage{
.imbalance_day_intervention(dataset, intervention, intervention_date, unexpected, alpha)
}
\arguments{
\item{dataset}{A \code{data.frame} of clinical trial data.}
\item{intervention}{Column name of column storing intervention status indicator.}
\item{intervention_date}{Column name of column storing intervention date.}
\item{unexpected}{List of elements specifying implausible values. Names of list are column names. One must be \code{"days"}.}
\item{alpha}{p-value signficance threshold.}
}
\description{
Internal function documentation for developers. Dates are converted into days of the week and tested for association to intervention status using \code{chisq.test}.
}
\value{
A list of length two. \code{check_table}: One-row \code{data.frame} with a Pass or Fail indicator. \code{images}: Bar chart of days of week. Bars are coloured by intervention status.
}
\examples{
library(readxl)
examplePath <- system.file("extdata", "dataset.xlsx", package = "integrity")
dataset <- read_excel(examplePath)
library(yaml)
infoPath <- system.file("extdata", "variables.yaml", package = "integrity")
info <- read_yaml(infoPath)
integrity:::.imbalance_day_intervention(dataset, info$intervention, info$enrollment$randomisation,
                                        info$unexpected, 0.05)
}