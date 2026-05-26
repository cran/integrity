\name{.prepare_data}
\alias{.prepare_data}
\title{Check clinical Data Matches its Data Specification}
\usage{
.prepare_data(dataset, info)
}
\arguments{
\item{dataset}{A \code{data.frame} of clinical trial data.}
\item{info}{A named list of column names corresponding to different aspects of the clinical trial.
See the vignette for detailed requirements.}
}
\description{
Internal function documentation for developers. Firstly, the function checks all expected variables are present as column names. Then, it converts any columns defined as categorical to factors. Finally, it removes any columns that have all missing values.
}
\value{
If no missing colums, a \code{data.frame} that has been filtered for columns containing all missing values.
}
\examples{
library(readxl)
examplePath <- system.file("extdata", "dataset.xlsx", package = "integrity")
dataset <- read_excel(examplePath)
library(yaml)
infoPath <- system.file("extdata", "variables.yaml", package = "integrity")
info <- read_yaml(infoPath)
integrity:::.prepare_data(dataset, info)
}