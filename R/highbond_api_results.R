
hb_api_get_result_name <- function(apikey, org, datacenter, table_id){
  # Pulls the name of the table (within an analysis, within a collection)
  
  # Construct path
  url <- paste0(hb_url(org, datacenter), 'tables/', table_id, '/')
  
  # Download
  name <- hb_api_get(apikey, url)
  
  # Return just a name
  name <- api_jsonParseDf(name)$data$attributes$name
  
  return(name)
}

hb_api_get_result_records <- function(apikey, org, datacenter, table_id){
  # Pulls the rows within a table (within an analysis, within a collection)
  
  # Construct path
  url <- paste0(hb_url(org, datacenter), 'tables/', table_id, '/records')
  
  # Download
  data <- hb_api_get(apikey, url)
  
  return(data)
}

hb_api_get_result_columns <- function(apikey, org, datacenter, table_id){
  # Pulls the rows within a table (within an analysis, within a collection)
  
  # Construct path
  url <- paste0(hb_url(org, datacenter), 'tables/', table_id, '/columns')
  
  # Download
  columns <- hb_api_get(apikey, url)
  
  # Return just a name
  columns <- api_jsonParseDf(columns)$data
  return(columns)
}

#' Retrieve Highbond Results table
#'
#' @description Downloads the content in the structure as saved by Highbond
#'   Results, including metadata and questionnaire responses (if applicable).
#'
#' @details Table ID can be found in the path of your Result table.
#'   https://<company_name>.results.highbond.com/projects/<COLLECTION_ID>/controls/<ANALYSIS_ID>/control_tests/<TABLE_ID>
#'
#'   Requires a Highbond API token. Activate a token from
#'   https://help.highbond.com/helpdocs/highbond/en-us/Content/launchpad/getting_started/managing_access_tokens.html.
#'
#'   The Organization number and datacenter can both be found from the Highbond launchpad, then accessing Options and Organization.
#'   https://accounts.highbond.com/orgs/<ORG_ID>/details
#'
#' @param apikey Highbond API token
#' @param org Organization number
#' @param datacenter One of \code{'us', 'ca', 'eu', 'ap', 'au'}
#' @param table_id The table to be downloaded
#' @param timezone Defaults to \code{Sys.timezone()}. See \code{OlsonNames()} for a list of allowable timezones.
#'
#' @importFrom dplyr mutate_at vars
#' @importFrom rlang .data
#' @importFrom lubridate as_datetime
#'
#' @return A list containing the contents, table requested and the original response
#' @export
#'
#' @examples
#' \dontrun{
#' response <- get_highbond_results(highbond_openapi, 12345, 'us', 567890)
#' }
get_highbond_results <- function(apikey, org, datacenter, table_id, timezone = Sys.timezone()){
  
  # Print table name
  tablename <- hb_api_get_result_name(apikey, org, datacenter, table_id)
  message(paste("Retrieving", tablename))
  
  # Download table
  resp <- hb_api_get_result_records(apikey, org, datacenter, table_id)
  parsed <- api_jsonParseDf(resp)
  
  # Separate records and column definition
  hbData <- parsed$data
  colMap <- data.frame(field_name = parsed$columns$field_name, type = parsed$columns$data_type)
  
  if((length(hbData) == 0)){
    hbData <- NULL
  } else {
    hbData <- hbData %>%
      mutate_at(vars(dplyr::filter(colMap, .data$type == 'datetime')$field_name), ~ as_datetime(., tz = timezone)) %>%
      mutate_at(vars(dplyr::filter(colMap, .data$type == 'numeric')$field_name), ~ as.numeric(.)) %>%
      mutate_at(vars(dplyr::filter(colMap, .data$type == 'logical')$field_name), ~ as.logical(.)) %>%
      mutate_at(vars(dplyr::filter(colMap, .data$type == 'date')$field_name), ~ as_datetime(., tz = timezone)) %>%
      mutate_at(vars(dplyr::filter(colMap, .data$type == 'character')$field_name), ~ as.character(.)) 
  }
  
  structure(
    list(
      content = list(data = hbData, columns = parsed$columns),
      table = list(table_id = table_id, table_name = tablename),
      response = resp
    ),
    class = 'highbond_results'
  )
}

#' Upload Highbond Results table
#'
#' @description Uploads the content in to Highbond Results.
#'
#' @details Current Results in the table can be purged or not. If data is not
#'   purged, a check is performed that compatible types are being upload and all
#'   fields exist. If purged, there will be no checks done beforehand, which may
#'   alter structure of existing table.
#'   
#'   If the Result table contains questionnaire responses, then any records that
#'   are being updated may need to have the responses also included as well.
#'   These fields are usually prefixed with a 'q'.
#'   
#'   A result row is generally appended, however a Primary Key field within the
#'   Data Analytic Settings for that table may be specified so a record with the
#'   same primary key may be merged, rather than duplicated.
#
#' @inheritParams get_highbond_results
#'
#' @param upload A data frame to be uploaded. Classes supported are
#'   c('character', 'numeric', 'logical', 'Date', 'POSIXct', 'POSIXlt')
#' @param purge FALSE by default. Whether or not to delete the contents of the table before overwriting.
#' @param skipquestions FALSE by default. Set TRUE if not purging and you don't want to overwrite the responses within the Results table already.
#' @param sizelimit 75000 bytes (~75 kb) by default. Used to estimate chunk size. Reduce size if upload chunks tend to fail.
#'
#' @return No data is returned, although errors will be verbose
#' @export
#' 
#' @importFrom dplyr mutate_if vars inner_join anti_join mutate
#' @importFrom utils object.size
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers
#' @importFrom utils capture.output
#'
#' @examples
#' \dontrun{
#'   upload <- data.frame(field_one = c('A','B','C'),
#'   field_two = c(1, 2, 3),
#'   field_three = c(TRUE, FALSE, TRUE),
#'   field_four = c(as.Date('2019-01-01'), as.Date('2020-01-01'), as.Date('2021-12-31')),
#'   field_five = c(as.POSIXct(Sys.time()), as.POSIXct(Sys.time()), as.POSIXct(Sys.time())))
#'   post_highbond_results(apikey, 12345, 'us', 567890, upload = upload, purge = TRUE)
#'   }
post_highbond_results <- function(apikey, org, datacenter, table_id, 
                                  upload = NULL, purge = FALSE, skipquestions = FALSE, sizelimit = 75000){
  
  if(is.null(upload) | !is.data.frame(upload)){
    stop('Please specify a dataframe to upload into Highbond Results')
  }
  
  # Print table name
  tablename <- hb_api_get_result_name(apikey, org, datacenter, table_id)
  message(paste("Uploading to", tablename))
  
  `%!in%` <- Negate('%in%')
  
  # Determine if any columns are not valid columns to be uploaded
  
  dfCols <- sapply(lapply(upload, class), function(x) x[1])
  
  if (any(dfCols %!in% c('character', 'numeric', 'logical', 'Date', 'POSIXct', 'POSIXlt'))){
    stop("Invalid data types to upload. Examine the classes in the dataframe.")
  }
  
  # Convert datetime to ISO 8701. Call it datetime for HB
  upload <- upload %>%
    mutate_if(lubridate::is.POSIXct, strftime, format = '%Y-%m-%dT%H:%M:%S%z') %>%
    mutate_if(lubridate::is.POSIXlt, strftime, format = '%Y-%m-%dT%H:%M:%S%z')
  
  dfCols <- gsub('posixct', 'datetime', tolower(dfCols))
  
  # Get and compare exisitng column specifications
  # TODO Add purge checks
  
  if(!purge){
    hbCols <- hb_api_get_result_columns(apikey, org, datacenter, table_id)
    
    hbCompare <- data.frame(hb_name = hbCols$id, hb_coltype = hbCols$attributes.data_type,
                            row.names = NULL,
                            stringAsFactors = FALSE)
    
    dfCompare <- data.frame(df_name = names(dfCols), col_type = unlist(dfCols),
                            row.names = NULL,
                            stringsAsFactors = FALSE)
    
    # If we're skipping the comparison on the questionnaire, remove any fields with a q
    if (skipquestions){
      hbCompare <- hbCompare %>%
        dplyr::filter(!grepl('[q][0-9]', .data$hb_name))
      
      dfCompare <- dfCompare %>%
        dplyr::filter(!grepl('[q][0-9]', .data$df_name))
    }
    
    missingLocalFields <- anti_join(dfCompare, hbCompare, by = c('df_name' = 'hb_name'))
    if (nrow(missingLocalFields) > 0){
      message(paste0("These fields are missing locally", capture.output(missingLocalFields), collapse = '\n'))
    }
    
    missingRemoteFields <- anti_join(hbCompare, dfCompare, by = c('hb_name' = 'df_name'))
    if (nrow(missingRemoteFields) > 0){
      message(paste0("These fields do not exist in the Highbond Results table", capture.output(missingRemoteFields), collapse = '\n'))
    }
    
    unmatchedFields <- inner_join(dfCompare, hbCompare, by = c('df_name' = 'hb_name'))
    unmatchedFields <- unmatchedFields %>%
      mutate(isMatchTypes = (.data$col_type == .data$hb_coltype)) %>%
      dplyr::filter(.data$isMatchTypes == FALSE)
    
    if (nrow(unmatchedFields) > 0){
      message(paste0("These fields are mismatched in type.", capture.output(unmatchedFields), collapse = '\n'))
    }
    
    if (nrow(missingLocalFields) + nrow(missingRemoteFields) + nrow(unmatchedFields)){
      stop("Please resolve before uploading", .call = TRUE)
    }
  }
  
  # Split datasets to upload
  size <- object.size(toJSON(upload))
  pages <- floor(as.numeric(size / sizelimit)) + 1
  
  uploadChunks <- list(pages) # Create list earlier for R resize list efficiency
  
  if (pages > 1){
    n <- nrow(upload)
    chunk <- ceiling(nrow(upload) / pages)
    reps <- rep(1:ceiling(n/chunk), each = chunk)[1:n]
    uploadChunks <- split(upload, reps)
  } else{
    uploadChunks[[1]] <- upload
  }
  
  # Upload
  i <- 1
  
  while (i <= length(uploadChunks)){
    uploadtoHB <- list()
    uploadtoHB$data$columns <- as.list(dfCols)
    uploadtoHB$data$records <- uploadChunks[[i]]
    uploadtoHB$options$purge <- purge
    
    url <- paste0(hb_url(org, datacenter), 'tables/', table_id, '/upload')
    
    message(paste('Uploading', i, 'of', length(uploadChunks), 'to', url))
    
    # Highbond example   
    # x <- '{"data":{"columns":{"field_one":"character","field_two":"character"},"records":[{ "field_one": "X","field_two": "2"},{"field_one": "XYZ","field_two": "676"}]},"options":{"purge":true}}'
    
    POST(url,
         body = toJSON(uploadtoHB, auto_unbox = TRUE),
         add_headers(Authorization = paste('Bearer', apikey),
                     `Content-Type` = 'application/json'),
         encoding = "raw")
    
    # Turn off purges
    purge <- FALSE
    i <- i + 1
  }
}