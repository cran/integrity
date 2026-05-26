\name{read_metadata_r}
\alias{read_metadata_r}
\title{Read Dataset Metadata from an R Script}
\usage{
read_metadata_r(path, object_name = "dataset_info")
}
\arguments{
\item{path}{Path to an R script containing a metadata object.}
\item{object_name}{Name of the object to return from the R script. Default: \code{"dataset_info"}.}
}
\description{
Sources an R script template and returns the metadata list required by
\code{run_checks()}.
}
\value{
A named \code{list} suitable for the \code{info} argument of \code{run_checks()}.
}
\examples{
if(interactive())
{
  example_path <- system.file("extdata", "variables_template.R", package = "integrity")
  dataset_info <- read_metadata_r(example_path)
  names(dataset_info)
}
}
