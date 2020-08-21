#' Retrieve Highbond Projects
#' 
#' @description Downloads the primary details of one or all projects
#'
#' @inheritParams get_highbond_results
#'
#' @param project_id Defaults to all projects
#' @param fields OPTIONAL. A character string of all fields requested within the project
#' @param pagesize Defaults to 50. Maximum is 100.
#'
#' @return A dataframe of projects
#' @export
#'
#' @examples
#' \dontrun{
#' projects <- get_project(highbond_openapi, highbond_org, highbond_datacenter)
#' }
get_project <- function(apikey, org, datacenter, project_id = NULL, fields = NULL, pagesize = 50){
  
  component <- 'projects' # Set up the project
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, project_id = project_id)) # Set up the query parameters
  
  plural <- is.null(project_id) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

hb_url_project <- function(component, project_id = NULL, objective_id = NULL, issue_id = NULL, id = NULL){
# Used to build the correct urls for Project information, depending on the type of data captured
# Future - perhaps put plural switch here?

  url <- NULL
  
  ### This needs to break out GET A and GET ALL 
  
  ### PART ONE - Substitution based on components. with no main area overriding the query
  
  # Project override as structure is mildly different  
  if (component == 'projects'){
      if (is.null(project_id) & is.null(id)) {
      url <- 'projects/' # Get all projects
    } else {
      url <- paste0('projects/', dplyr::if_else(!is.null(project_id), project_id, id)) # Get one specific project
    }
    
    return(url) # Early return
  }
  
  # Project Types override, different because the ID is different
  if (component == 'projects_types'){
    if (is.null(id)) {
      url <- 'projects_types/' # Get all projects
    } else {
      url <- paste0('projects_types/', id) # Get one specific project type
    }
    
    return(url) # Early return
  }
  
  # These have no higher level component, but ID is always required
  if(component %in% c('control_test_plans', 'walkthroughs', 'control_tests', 'mitigations')){
    url <- paste0(component, '/', id)
    
    return(url) # Early return
  }
  
  # Error check to make sure enough information is specified to proceed into next stage
  
  if ((is.null(project_id) & is.null(objective_id) & is.null(issue_id)) &
      (is.null(id))) # If the project or objective isn't specified, then at least one of the components needs to be
  {
    stop("(Project & Objective & Issue) and (ID) can't be empty for requested component")
  }
  
  ### PART TWO - Substitution based on components and main area
  
  # Project based components
  if(component %in% c('planning_files', 'result_files', 'objectives', 'issues')){
    if (is.null(id)){
      url <- paste0('projects/', project_id, '/', component)
        } else {
      url <- paste0(component, '/', id)
        }
  }
  
  # Objective based components
  if(component %in% c('narratives', 'risks', 'controls')){
    if (is.null(id)){
      url <- paste0('objectives/', objective_id, '/', component)
    } else {
      url <- paste0(component, '/', id)
    }
  }
  
  # Issue based components
  if(component %in% c('actions')){
    if (is.null(id)){
      url <- paste0('issues/', issue_id, '/', component)
    } else {
      url <- paste0(component, '/', id)
    }
  }
  
  # Final check
  if (is.null(url)){
    stop("(Project & Objective & Issue) and (Component) can't be empty, but something broke")
  }
  
  return(url)
}

hb_prj_set_params <- function(component, pagesize, fields = NULL){
  # Creates a list of eligible parameters based on the generalized requirements
  # The check for using this should occur at the component-check level
  
  # ATTEMPT 2 - Inspired by Chel Hee Lee's advice
  # Use this method that helps force generally illegal names to be assigned to the list, which keeps its format when calling GET query later (via params)
  # Modify by attempting to force names as list name, as the substitution/eval method doesn't allow for NAs, which are possible
  # txt1 <- paste("param_test <- list(
  #   `fields[", component, "] = '", fields, "',
  #  `page[size]` = 2
  #   )")
  # 
  # print(eval(parse(text=txt1)))
  
  param_list <- list()
  param_name_pagesize <- paste0('page[size]')
  param_name_fields <- paste0('fields[',component,']')
  param_list[param_name_pagesize] <- pagesize
  param_list[param_name_fields] <- fields
  
  return(param_list)
}

#' @importFrom rlang .data
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyjson spread_values enter_object gather_object spread_all jstring
hb_prj_parse_standard <- function(content_raw){
  # This will parse the standard data fields that exist in all get points
  
  content_header <- content_raw %>%
    tidyjson::spread_values(id = tidyjson::jstring(id),
                  type = tidyjson::jstring(type)) %>%
    #  spread_all %>%
    select(.data$document.id, .data$id, .data$type) %>%
    as_tibble()
  
  content_attributes <- content_raw %>% 
    tidyjson::enter_object(attributes) %>% 
    spread_all %>%
    as_tibble()
  
  content_rel <- content_raw %>%
    tidyjson::enter_object(relationships) %>%
    gather_object %>%
    spread_all %>% 
    select(.data$document.id, .data$name, relationship_id = .data$data.id, relationship_type = .data$data.type) %>%
    as_tibble() %>%
    nest(relationships = c(.data$name, .data$relationship_id, .data$relationship_type))
  
  content_parts <- tibble(
    header = content_header,
    attributes = content_attributes,
    relationships = content_rel
  )
  
  return(content_parts)
}

#' @importFrom rlang .data
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyjson spread_values enter_object gather_object gather_array spread_all append_values_string
hb_prj_parse_custom <- function(component, content_raw){
  # This parses custom only
  
  if(!(component %in% c('projects'))){
    return(NULL)
  }
  
  content_attributes_custom <- content_raw %>% 
    enter_object(attributes) %>% 
    enter_object(custom_attributes) %>%
    gather_array() %>%
    gather_object() %>% 
    append_values_string("value") %>%
    select(.data$document.id, .data$array.index, .data$name, .data$value) %>% #propose to remove array.index
    as_tibble() %>%
    nest(custom_attributes = c(.data$array.index, .data$name, .data$value)) # CONSIDER RESHAPING TO HAVE ID, NAME, VALUE
  
  return(content_attributes_custom)
}

#' @importFrom rlang .data
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyjson spread_values enter_object gather_object gather_array spread_all append_values_string
hb_prj_parse_tags <- function(component, content_raw){
  # This only processes tags
  
  if(!(component %in% c('projects'))){
    return(NULL)
  }
  
  content_tags <- content_raw %>% 
    tidyjson::enter_object(attributes) %>% 
    tidyjson::enter_object(tag_list) %>% 
    gather_array() %>% 
    append_values_string() %>%
    select(.data$document.id, .data$array.index, tag_list = .data$string) %>% #propose to remove array.index
    as_tibble() %>%
    nest(tag_list = c(.data$array.index, .data$tag_list))
  
  return(content_tags)
}

#' @importFrom rlang .data
#' @importFrom dplyr select left_join
hb_prj_coljoin_data <- function(component, core, custom, tags){
  . <- NULL
  
  # Combine all the data together
  
  joined <- core$header %>%
    left_join(core$attributes, by = 'document.id') %>% 
    {
      if(component %in% c('projects')){
        left_join(., custom, by = 'document.id')
      }} %>% {
        if(component %in% c('projects')){
          left_join(., tags, by = 'document.id')
        }} %>%
    left_join(core$relationships, by = 'document.id') %>%
    select(-.data$document.id)
  
  return(joined)
}

#' @importFrom rlang .data
#' @importFrom dplyr select bind_rows
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
hb_prj_get_controller <- function(apikey, url, params, datacenter, component, plural, waittime = 0.6){
 # Performs the GET and loop
  
   i <- 1 # pointer

  # Download the first page, specifying parameters
  download <- hb_api_get(apikey, url, waittime, params = params)
  json <- httr::content(download, 'text')
  content <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  next_page <- content$links$`next` # Save next page reference
  content_data <- if(plural){content$data} else {content} # This is important for many

  core <- hb_prj_parse_standard(content_data) # Returns the three primary tables in all - header, attributes, relationships

  # Custom fields get - relevant to most except...
  custom <- hb_prj_parse_custom(component, content_data)
  tags <- hb_prj_parse_tags(component, content_data)

  # First page, finished
  combined_data <- hb_prj_coljoin_data(component, core, custom, tags)
  
  # Pagination
  while (!is.null(next_page)){
    combined_data_next <- NULL # Just a clean reset
    i <- i + 1 # increment to save into next page
    
    url <- paste0(hb_url_base(datacenter),next_page) # Use the next page as specified
    download <- hb_api_get(apikey, url, waittime) # Don't use parameters anymore
    
    json <- httr::content(download, 'text')
    content <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    
    next_page <- content$links$`next` # Save next page reference
    
    content_data <- if(plural){content$data} else {content} # This is important for many
    
    core <- hb_prj_parse_standard(content_data) # Returns the three primary tables in all - header, attributes, relationships
    
    # Custom fields get - relevant to most except...
    custom <- hb_prj_parse_custom(component, content_data)
    tags <- hb_prj_parse_tags(component, content_data)
    
    # Next page, finished
    combined_data_next <- hb_prj_coljoin_data(component, core, custom, tags)
    
    combined_data <- bind_rows(combined_data, combined_data_next)
  }

  return(combined_data)
}





# All the specific data below only works with active projects

## Project based queries

#/orgs/{org_id}/projects/{project_id}/planning_files
#/orgs/{org_id}/planning_files/{id} ### -- PROJECT ID
#/orgs/{org_id}/projects/{project_id}/results_files
#/orgs/{org_id}/results_files/{id}
#/orgs/{org_id}/projects/{project_id}/objectives # THIS ACTS AS BASIS FOR OBJECTIVE BASED QUERIES
#/orgs/{org_id}/objectives/{id} 
#/orgs/{org_id}/projects/{project_id}/issues
#/orgs/{org_id}/issues/{id}

## Objective based queries (can be sections, processes, cycles, functional areas, application systems)

#/orgs/{org_id}/objectives/{objective_id}/narratives # INTERNAL CONTROLS ONLY
#/orgs/{org_id}/narratives/{id} # INTERNAL CONTROLS ONLY
#/orgs/{org_id}/objectives/{objective_id}/risks
#/orgs/{org_id}/risks/{id}
#/orgs/{org_id}/objectives/{objective_id}/controls #IC Controls, Workplan Procedures
#/orgs/{org_id}/controls/{id}  ## THIS LINKS OUT TO CONTROL_TESTS via:
## data -- relationships
### -- walkthrough, control_test_plan, control_tests, mitigations

## Controls ID relationship based queries
#/orgs/{org_id}/mitigations
#/orgs/{org_id}/control_test_plans/{id} 
#/orgs/{org_id}/walkthroughs/{id}  # IC Walkthroughts, Workplan Execute Procedure
#/orgs/{org_id}/control_tests/{id}  # INTERNAL CONTROLS ONLY

## Issue ID based queries (Issues is above)
#/orgs/{org_id}/issues/{issue_id}/actions
#/orgs/{org_id}/actions/{id}

### CAN NOT TELL HOW TO GET REQUEST ITEM BY PROJECT OR ID.
### Only one way out of requests, but you need to know the ID first
#/orgs/{org_id}/request_items/{id}

### Generic Highbond Settings
#/orgs/{org_id}/entities
#/orgs/{org_id}/entities/{id}
#/orgs/{org_id}/entity_categories
#/orgs/{org_id}/entity_categories/{id}

# data - relationships - project - data - id

# when posting, how to tell which project to associate it to?

# get_projecttype <- function(){
#   #/orgs/{org_id}/project_types
#   #/orgs/{org_id}/project_types/{id}
# }

#https://docs-apis.highbond.com/#operation/getProjects
