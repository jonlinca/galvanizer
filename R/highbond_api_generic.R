# Documentation for API at: https://docs-apis.highbond.com/

hb_api_get <- function(apikey, url, waittime = 0.6, params = NULL){
  # The function that actually pulls Highbond data
  
  if (is.null(params)){
    hb_api_get <- httr::GET(url, 
                            hb_headers(apikey))
  } else {
    # We have to do this because having a params of blank but passing a fully encoded url will clear all the parameters
    hb_api_get <- httr::GET(url, 
                          hb_headers(apikey),
                          query = params)
  }
  
  Sys.sleep(waittime) # Wait time is required as Highbond limits rates to about three queries per second
  hb_validateDownload(hb_api_get)
  return(hb_api_get)
}

hb_headers <- function(apikey){
  hb_headers <- httr::add_headers(Authorization = paste("Bearer", apikey),
                            `Content-Type` = 'application/vnd.api+json',
                            `Accept-Encoding` = '')
  
  return(hb_headers) 
}

hb_validateDownload <- function(content){
  httr::warn_for_status(content)
  httr::stop_for_status(content)
  
  if (httr::http_type(content) != 'application/json' & httr::http_type(content) != 'application/vnd.api+json'){
    stop("API did not return application/json or application/vnd.api+json", call. = FALSE)
  }
}

hb_url <- function(org, datacenter){
  # See https://docs-apis.highbond.com/#section/Making-requests
  
  regionurl <- hb_url_base(datacenter) # Moved to enable hb_url_base functionality to accomodate projects. Same syntax kept to allow for Results to continue without re-engineering
  
  url <- paste0(regionurl, '/v1/orgs/', org, '/')
  
  return(url)
}

hb_url_base <- function(datacenter){
  # This needs to be separate due to needing a base URL for Projects and next page handler
  
  if (datacenter == 'us'){
    regionurl <- 'https://apis.highbond.com'
  } else if (datacenter == 'ca'){
    regionurl <- 'https://apis-ca.highbond.com'
  } else if (datacenter == 'eu'){
    regionurl <- 'https://apis-eu.highbond.com'
  } else if (datacenter == 'ap'){
    regionurl <- 'https://apis-ap.highbond.com'
  } else if (datacenter == 'au'){
    regionurl <- 'https://apis-au.highbond.com'
  } else {
    message("Unspecified datacenter - defaulting to 'us'")
    regionurl <- 'https://apis.highbond.com'
  }
  
  return(regionurl)
}

api_jsonParseDf <- function(apicontent){
  # Flattens data into a list
  
  parsed <- jsonlite::fromJSON(httr::content(apicontent, 'text'),
                               simplifyVector = FALSE,
                               simplifyDataFrame = TRUE,
                               flatten = TRUE)
  
  return(parsed)
}

