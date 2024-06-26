% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rorcid.R
\name{get_cv_data}
\alias{get_cv_data}
\title{get CV data from ORCID}
\usage{
get_cv_data(
  orcid = Sys.getenv("ORCID_ID"),
  json_path = NULL,
  entries = c("education", "employment", "citation")
)
}
\arguments{
\item{orcid}{string with the ORCID ID
by default fetched fomr "ORCID_ID" system variable}

\item{json_path}{string to JSON file with additional/custom data}

\item{entries}{character vector with entries to be included in the CV}
}
\value{
list with CV data
}
\description{
get CV data from ORCID
}
\keyword{CV}
