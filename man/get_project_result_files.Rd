% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects.R
\name{get_project_result_files}
\alias{get_project_result_files}
\title{Retrieve Highbond Project - Result Files}
\usage{
get_project_result_files(
  auth,
  project_id = NULL,
  results_file_id = NULL,
  fields = NULL,
  pagesize = 50,
  waittime = 0.2
)
}
\arguments{
\item{auth}{Highbond authentication credentials, created from \code{\link{setup_highbond}}}

\item{project_id}{Required if other parameter is blank. May obtain multiple rows.}

\item{results_file_id}{Required if other parameter is blank. Will get only one row.}

\item{fields}{OPTIONAL. A character vector each field requested within the
project. NULL will default to all fields.}

\item{pagesize}{Defaults to 50. Maximum is 100.}

\item{waittime}{Time in seconds to wait between requests.}
}
\value{
A tibble of result files
}
\description{
Downloads the primary details of one or multiple result files
for a project.
}
\details{
possible fields: name, reference_id, description, position,
created_at, updated_at, custom_attributes, project
}
