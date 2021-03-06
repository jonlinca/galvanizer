% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects.R
\name{get_project_objectives}
\alias{get_project_objectives}
\title{Retrieve Highbond Project - Objectives}
\usage{
get_project_objectives(
  auth,
  project_id = NULL,
  objective_id = NULL,
  fields = NULL,
  pagesize = 50,
  waittime = 0.2
)
}
\arguments{
\item{auth}{Highbond authentication credentials, created from \code{\link{setup_highbond}}}

\item{project_id}{Required if other parameter is blank. May obtain multiple rows.}

\item{objective_id}{Required if other parameter is blank. Will get only one row.}

\item{fields}{OPTIONAL. A character vector each field requested within the
project. NULL will default to all fields.}

\item{pagesize}{Defaults to 50. Maximum is 100.}

\item{waittime}{Time in seconds to wait between requests.}
}
\value{
A tibble of objectives
}
\description{
Downloads the primary details of one or multiple objectives for
a project. Also known as sections, processes, cycles, functional areas,
application systems, or another custom term.
}
\details{
possible fields: title, description, reference, division_department,
owner, executive_owner, created_at, updated_at, project, assigned_user,
custom_attributes, position, risk_control_matrix_id,
walkthrough_summary_id, testing_round_1_id, testing_round_2_id,
testing_round_3_id, testing_round_4_id, entities
}
