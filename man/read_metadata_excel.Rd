\name{read_metadata_excel}
\alias{read_metadata_excel}
\title{Read Dataset Metadata from an Excel Template}
\usage{
read_metadata_excel(path, sheet = 1)
}
\arguments{
\item{path}{Path to an Excel workbook containing metadata rows.}
\item{sheet}{Sheet name or position to read. Default: 1.}
}
\description{
Reads a metadata workbook template and converts it into the named list structure
required by \code{run_checks()}.
}
\value{
A named \code{list} suitable for the \code{info} argument of \code{run_checks()}.
}
\examples{
if(interactive())
{
  example_path <- system.file("extdata", "variables_template.xlsx", package = "integrity")
  dataset_info <- read_metadata_excel(example_path)
  names(dataset_info)
}
}
