hb_api_get_result_name <- function(auth, table_id){
  # Pulls the name of the table (within an analysis, within a collection)
  
  # Construct path
  url <- paste0(hb_url(auth), 'tables/', table_id, '/')
  
  # Download
  name <- hb_api_get(auth, url)
  
  # Return just a name
  name <- api_jsonParseDf(name)$data$attributes$name
  
  return(name)
}

hb_api_get_result_records <- function(auth, table_id){
  # Pulls the rows within a table (within an analysis, within a collection)
  
  # Construct path
  url <- paste0(hb_url(auth), 'tables/', table_id, '/records')
  
  # Download
  data <- hb_api_get(auth, url)
  
  return(data)
}

hb_api_get_result_columns <- function(auth, table_id){
  # Pulls the rows within a table (within an analysis, within a collection)
  
  # Construct path
  url <- paste0(hb_url(auth), 'tables/', table_id, '/columns')
  
  # Download
  columns <- hb_api_get(auth, url)
  
  # Return just a name
  columns <- api_jsonParseDf(columns)$data
  return(columns)
}