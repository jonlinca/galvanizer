% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic.R
\name{setup_highbond}
\alias{setup_highbond}
\title{Highbond Authentication credentials}
\usage{
setup_highbond(apikey, instance, datacenter)
}
\arguments{
\item{apikey}{Highbond API token}

\item{instance}{The HighBond instance ID, also known as the organization number}

\item{datacenter}{The region code. Can be \code{'us', 'ca', 'eu', 'ap', 'au'}}
}
\value{
A Highbond Authentication credentials object to pass to your requests
}
\description{
Assembles all the core authentication needed to connect to a Highbond instance.
}
\details{
Requires a Highbond API token. Activate a token from
\url{https://help.highbond.com/helpdocs/highbond/en-us/Content/launchpad/getting_started/managing_access_tokens.html}.

The Instance (Organization) number and datacenter can both be found from the Highbond launchpad, then accessing Options and Organization.
\url{https://accounts.highbond.com/orgs/<ORG_ID>/details}
}
