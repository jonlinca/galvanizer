
<!-- README.md is generated from README.Rmd. Please edit that file -->

# galvanizer

<!-- badges: start -->

<!-- badges: end -->

galvanizer is a package designed to interact to Highbond’s API
(<https://docs-apis.highbond.com>). By using R with galvanizer, you can
download data from Highbond, process them locally, and upload any new
results into Highbond Results.

## Installation

You can install the currently available version of galvanizer from
[github](https://github.com/jonlinca/galvanizer) with:

``` r
# install.packages("devtools")
devtools::install_github("jonlinca/galvanizer")
```

## Getting started

Before using the API, you will need some core information to access your
Highbond instance. They are:

  - Highbond API token
  - Organization number
  - Data Center location

To generate a Highbond API token from your Highbond instance by
referencing the [help
files](https://help.highbond.com/helpdocs/highbond/en-us/Content/launchpad/getting_started/managing_access_tokens.html).
This can be found on your profile page.

![generate API token](man/figures/generate_token.png)

To identify your organization number and data center, best found from
your Launchpad, under Options / Organization. The organization number
will be under the format of
`https://accounts.highbond.com/orgs/<ORGANIZATION_ID>/details`

## Highbond Results data

Once you have those, then you need to identify the table you want to
upload and download data from. The easiest way to do this is to navigate
to the Results Data Analytic table you want to upload and grab the
`TABLE_ID` from the URL. It will look like:

`https://<company_name>.results.highbond.com/projects/<COLLECTION_ID>/controls/<ANALYSIS_ID>/control_tests/<TABLE_ID>`

You can choose to upload a set of test data into a new Data Analytic to
try this out:

``` r
library(galvanizer)

upload <- data.frame(name = c('Cowbell', 'Rufus'),
                     age = c(21, 32),
                     active = c(TRUE, FALSE),
                     date_started = c(as.Date('2019-01-01'), as.Date('2020-01-01')),
                     date_ended = c(as.POSIXct(Sys.time()), as.POSIXct(Sys.time())))

# Substitute your API key, org number, data, center, table, and the dataframe to be uploaded
post_highbond_results(highbond_openapi, highbond_org, highbond_datacenter, highbond_table, upload = upload, purge = TRUE)
```

Once that has successfully uploaded into Highbond Results, you can view
the results online and also download the new data too:

![successful results upload](man/figures/highbond_results_upload.png)

``` r
download <- get_highbond_results(highbond_openapi, highbond_org, highbond_datacenter, highbond_table)
#> Retrieving Testthat R Post
#> Date in ISO8601 format; converting timezone from UTC to "America/Denver".
#> Date in ISO8601 format; converting timezone from UTC to "America/Denver".
#> Date in ISO8601 format; converting timezone from UTC to "America/Denver".

head(upload)
#>      name age active date_started          date_ended
#> 1 Cowbell  21   TRUE   2019-01-01 2020-07-17 11:55:06
#> 2   Rufus  32  FALSE   2020-01-01 2020-07-17 11:55:06
```