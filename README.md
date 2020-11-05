
<!-- README.md is generated from README.Rmd. Please edit that file -->

# galvanizer

<!-- badges: start -->

<!-- badges: end -->

galvanizer is a package designed to interact to Highbond’s API
(<https://docs-apis.highbond.com>). By using R with galvanizer, you can
download data from Highbond’s Results and Project modules. For Highbond
Results, you can also process data locally and upload data into Highbond
Result tables.

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

To use any functions, you will need to create and pass a Highbond
Authentication object. You can make it by passing the API token,
organization / instance number and data center location to
`setup_highbond()`:

``` r
library(galvanizer)
highbond_auth <- setup_highbond(highbond_openapi, highbond_org, highbond_datacenter)
```

## Highbond Results data

Once you have those, then you need to identify the table you want to
upload and download data from. The easiest way to do this is to navigate
to the Results Data Analytic table you want to upload and grab the
`TABLE_ID` from the URL. It will look like:

`https://<company_name>.results.highbond.com/projects/<COLLECTION_ID>/controls/<ANALYSIS_ID>/control_tests/<TABLE_ID>`

You can choose to upload a set of test data into a new Data Analytic to
try this out:

``` r
upload <- data.frame(name = c('Cowbell', 'Rufus'),
                     age = c(21, 32),
                     active = c(TRUE, FALSE),
                     date_started = c(as.Date('2019-01-01'), as.Date('2020-01-01')),
                     date_ended = c(as.POSIXct(Sys.time()), as.POSIXct(Sys.time())))

# Substitute your API key, org number, data, center, table, and the dataframe to be uploaded
post_results_records(highbond_auth, highbond_table, upload = upload, purge = TRUE)
```

Once that has successfully uploaded into Highbond Results, you can view
the results online and also download the new data too:

![successful results upload](man/figures/highbond_results_upload.png)

``` r
download <- get_results_records(highbond_auth, highbond_table)
#> Retrieving Readme Table
#> Date in ISO8601 format; converting timezone from UTC to "America/Edmonton".
#> Date in ISO8601 format; converting timezone from UTC to "America/Edmonton".
#> Date in ISO8601 format; converting timezone from UTC to "America/Edmonton".

head(download$content$data)
#>   metadata.priority metadata.status metadata.publish_date metadata.publisher
#> 1               Low             New   2020-11-02 11:45:37           Jon Test
#> 2               Low             New   2020-11-02 11:45:37           Jon Test
#>   metadata.assignee metadata.group metadata.updated_at metadata.closed_at
#> 1              <NA>           <NA> 2020-11-02 11:45:37               <NA>
#> 2              <NA>           <NA> 2020-11-02 11:45:37               <NA>
#>      name age active date_started          date_ended extras.record_id
#> 1 Cowbell  21   TRUE   2019-01-01 2020-11-02 11:45:05     1.094678e+15
#> 2   Rufus  32  FALSE   2020-01-01 2020-11-02 11:45:05     1.094678e+15
#>   extras.collection extras.results_table
#> 1 galvanizer checks         Readme Table
#> 2 galvanizer checks         Readme Table
```

## Highbond Projects data

Highbond Projects data (also known as ‘Audits’) is also retrievable,
depending on which screen of information you would like to retrieve
from. There are options to download all or just one set of information,
depending on the pane chosen.

``` r
projects <- get_project(highbond_auth)
head(projects)
#> # A tibble: 6 x 26
#>   id    type  name  state status created_at updated_at description background
#>   <chr> <chr> <chr> <chr> <chr>  <chr>      <chr>      <chr>       <chr>     
#> 1 1213… proj… Payr… acti… active 2020-08-1… 2020-08-2… "Project t… <NA>      
#> 2 1213… proj… IT G… acti… active 2020-08-1… 2020-08-1… ""          <NA>      
#> 3 1213… proj… Sarb… acti… active 2020-08-1… 2020-08-2… ""          <NA>      
#> 4 1213… proj… Sale… acti… active 2020-08-1… 2020-08-1… "Project t… <NA>      
#> 5 1213… proj… SOC … acti… active 2020-08-1… 2020-08-1… "Project t… <NA>      
#> 6 1213… proj… Purc… acti… active 2020-08-1… 2020-08-1… "The purch… <NA>      
#> # … with 17 more variables: budget <dbl>, position <dbl>, certification <lgl>,
#> #   control_performance <lgl>, risk_assurance <lgl>, management_response <chr>,
#> #   max_sample_size <dbl>, number_of_testing_rounds <dbl>, opinion <chr>,
#> #   opinion_description <chr>, purpose <chr>, scope <chr>, start_date <chr>,
#> #   target_date <chr>, custom_attributes <list>, tag_list <list>,
#> #   relationships <list>
```
