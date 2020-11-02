# Documentation for API at: https://docs-apis.highbond.com/

#' Highbond Authentication credentials
#' 
#' Assembles all the core authentication needed to connect to a Highbond instance.
#' 
#' @details 
#' 
#'   Requires a Highbond API token. Activate a token from
#'   \url{https://help.highbond.com/helpdocs/highbond/en-us/Content/launchpad/getting_started/managing_access_tokens.html}.
#'
#'   The Instance (Organization) number and datacenter can both be found from the Highbond launchpad, then accessing Options and Organization.
#'   \url{https://accounts.highbond.com/orgs/<ORG_ID>/details}
#'
#' @param apikey Highbond API token
#' @param instance The HighBond instance ID, also known as the organization number
#' @param datacenter The region code. Can be \code{'us', 'ca', 'eu', 'ap', 'au'}
#'
#' @return A Highbond Authentication credentials object to pass to your requests
#' @export
setup_highbond <- function(apikey, instance, datacenter){

  # Check data center id
  `%!in%` = Negate(`%in%`)
  
  if (datacenter %!in% c('us', 'ca', 'eu', 'ap', 'au')){
    stop("Unknown data center region.")
  }
  
  # Make sure the org ID is a number
  if (as.numeric(instance) != instance){
    stop("Instance/Organization should be a numeric value.")
  }
  
  # Create stable class of highbond authentication object
  structure(
    list(key = apikey,
         org = instance,
         dc = datacenter),
    class = 'hb_auth')
}

hb_checkauth <- function(auth){
  stopifnot(class(auth) == 'hb_auth')
}

hb_api_get <- function(auth, url, waittime = 0.6, params = NULL){
  # The function that actually pulls Highbond data
  hb_checkauth(auth)
  
  apikey <- auth$key
  
  if (is.null(params)){
    hb_api_get <- httr::GET(url, 
                            hb_headers(auth))
  } else {
    # We have to do this because having a params of blank but passing a fully encoded url will clear all the parameters
    hb_api_get <- httr::GET(url, 
                          hb_headers(auth),
                          query = params)
  }
  
  Sys.sleep(waittime) # Wait time is required as Highbond limits rates to about three queries per second
  hb_validateDownload(hb_api_get)
  return(hb_api_get)
}

hb_headers <- function(auth){
  hb_checkauth(auth)
  
  apikey <- auth$key
  
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

hb_url <- function(auth){
  # See https://docs-apis.highbond.com/#section/Making-requests
  hb_checkauth(auth)
  
  regionurl <- hb_url_base(auth) # Moved to enable hb_url_base functionality to accomodate projects. Same syntax kept to allow for Results to continue without re-engineering
  
  url <- paste0(regionurl, '/v1/orgs/', auth$org, '/')
  
  return(url)
}

hb_url_base <- function(auth){
  # This needs to be separate due to needing a base URL for Projects and next page handler
  hb_checkauth(auth)

  datacenter <- auth$dc
    
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

