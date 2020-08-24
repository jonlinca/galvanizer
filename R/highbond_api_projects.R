#' Retrieve Highbond Projects - Projects
#'
#' @description Downloads the primary details of one or all projects
#'
#' @details Fields allowed: name, state, status, created_at, updated_at,
#'   description, background, budget, position, certification,
#'   control_performance, risk_assurance, management_response, max_sample_size,
#'   number_of_testing_rounds, opinion, opinion_description, purpose, scope,
#'   start_date, target_date, tag_list, project_type, entities
#'
#' @inheritParams get_highbond_results
#'
#' @param project_id Project ID number. \code{NULL} will default to all items.
#' @param fields OPTIONAL. A character vector each field requested within the
#'   project. NULL will default to all fields.
#' @param pagesize Defaults to 50. Maximum is 100.
#'
#' @return A tibble of projects
#' @export
#'
#' @examples
#' \dontrun{
#' projects <- get_project(highbond_openapi, highbond_org, highbond_datacenter)
#' projects <- get_project(highbond_openapi, highbond_org, highbond_datacenter, 
#'   fields = c('name', 'state', 'status'))
#' }
get_project <- function(apikey, org, datacenter, project_id = NULL, fields = NULL, pagesize = 50){
  
  primary <- project_id
  component <- 'projects' # Set up the project
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary)) # Set up the query parameters
  plural <- is.null(primary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Projects - Project Types
#'
#' @description Downloads the primary details of one or all project types.
#'
#' @details Possible fields include: name, description, workflow, project_terms,
#'   certification_terms, control_terms, finding_terms, control_test_terms,
#'   finding_action_terms, narrative_terms, objective_terms, planning_terms,
#'   results_terms, risk_terms, test_plan_terms, walkthrough_terms
#'
#' @inheritParams get_project
#'
#' @param project_type_id \code{NULL} Defaults to all project types
#'
#' @export
#' @return A tibble of project types
get_project_type <- function(apikey, org, datacenter, project_type_id = NULL, fields = NULL, pagesize = 50){
  
  primary <- project_type_id
  component <- 'project_types' # Set up the project
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary)) # Set up the query parameters
  plural <- is.null(primary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Planning Files
#'
#' @description Downloads the primary details of one or multiple planning files
#'   for a project. 
#'
#' @details possible fields: name, reference_id, description, position,
#'   created_at, updated_at, custom_attributes, project
#'
#' @inheritParams get_project
#'
#' @param project_id Required if other parameter is blank. May obtain multiple rows.
#' @param planning_file_id Required if other parameter is blank. Will get only one row.
#' 
#' @export
#' @return A tibble of planning files.
get_project_planning_file <- function(apikey, org, datacenter, project_id = NULL, planning_file_id = NULL, fields = NULL, pagesize = 50){

  primary <- project_id
  secondary <- planning_file_id
  component <- 'planning_files' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download

  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Result Files
#'
#' @description Downloads the primary details of one or multiple result files
#'   for a project.
#'
#' @details possible fields: name, reference_id, description, position,
#'   created_at, updated_at, custom_attributes, project
#'
#' @inheritParams get_project
#'
#' @param project_id Required if other parameter is blank. May obtain multiple rows.
#' @param results_file_id Required if other parameter is blank. Will get only one row.
#' @export
#' @return A tibble of result files
get_project_results_file <- function(apikey, org, datacenter, project_id = NULL, results_file_id = NULL, fields = NULL, pagesize = 50){
  
  primary <- project_id
  secondary <- results_file_id
  component <- 'results_files' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Objectives
#'
#' @description Downloads the primary details of one or multiple objectives for
#'   a project. Also known as sections, processes, cycles, functional areas,
#'   application systems, or another custom term.
#'
#' @details possible fields: title, description, reference, division_department,
#'   owner, executive_owner, created_at, updated_at, project, assigned_user,
#'   custom_attributes, position, risk_control_matrix_id,
#'   walkthrough_summary_id, testing_round_1_id, testing_round_2_id,
#'   testing_round_3_id, testing_round_4_id, entities
#'
#' @inheritParams get_project
#'
#' @param project_id Required if other parameter is blank. May obtain multiple rows.
#' @param objective_id Required if other parameter is blank. Will get only one row.
#' @export
#' @return A tibble of objectives
get_project_objective <- function(apikey, org, datacenter, project_id = NULL, objective_id = NULL, fields = NULL, pagesize = 50){
  
  primary <- project_id
  secondary <- objective_id
  component <- 'objectives' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Narratives
#'
#' @description "A narrative is a description of a business process or area under
#'   review. Narratives are also known as policies, process narratives, process
#'   descriptions, or control guides."
#'   
#' @details possible fields: title, description, created_at, updated_at, objective
#'
#' @inheritParams get_project
#'
#' @param objective_id Required if other parameter is blank. May obtain multiple rows.
#' @param narrative_id Required if other parameter is blank. Will get only one row.
#' @export
#' @return A tibble of narratives
get_project_narrative <- function(apikey, org, datacenter, objective_id = NULL, narrative_id = NULL, fields = NULL, pagesize = 50){
  
  primary <- objective_id
  secondary <- narrative_id
  component <- 'narratives' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download

  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Risks
#'
#' @description "A narrative is a description of a business process or area
#'   under review. Narratives are also known as policies, process narratives,
#'   process descriptions, or control guides."
#'
#' @details possible fields: title, description, risk_id, position, impact,
#'   likelihood, custom_attributes, created_at, updated_at, objective,
#'   mitigations, entities
#'
#' @inheritParams get_project
#'
#' @param objective_id Required if other parameter is blank. May obtain multiple rows.
#' @param risk_id Required if other parameter is blank. Will get only one.
#' @export
#' @return A tibble of Risks
get_project_risk <- function(apikey, org, datacenter, objective_id = NULL, risk_id = NULL, fields = NULL, pagesize = 50){
  
  primary <- objective_id
  secondary <- risk_id
  component <- 'risks' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Controls
#'
#' @description "A control is a program, policy, routine, or activity that is
#'   intended to mitigate a risk. Controls are organized by objectives, and can
#'   be associated with one or more risks. The combination of identified risks
#'   and corresponding controls defines the Risk Control Matrix. Controls are
#'   also known as procedures."
#'
#' @details possible fields: title, description, control_id, owner, frequency,
#'   control_type, prevent_detect, method, status, position, created_at,
#'   updated_at, custom_attributes, objective, walkthrough, control_test_plan,
#'   control_tests, mitigations, owner_user, entities
#'
#' @inheritParams get_project
#'
#' @param objective_id Required if other parameter is blank. May obtain multiple rows.
#' @param control_id Required if other parameter is blank. Will get only one.
#'  
#' @export
#' @return A tibble of controls
get_project_control <- function(apikey, org, datacenter, objective_id = NULL, control_id = NULL, fields = NULL, pagesize = 50){
  
  primary <- objective_id
  secondary <- control_id
  component <- 'controls' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Mitigations
#'
#' @description The mapping between controls and risks
#'
#' @inheritParams get_project
#'
#' @param mitigation_id Will get only one.
#'  
#' @export
#' @return A tibble of mitigations
get_project_mitigation <- function(apikey, org, datacenter, mitigation_id, fields = NULL, pagesize = 50){
  
  primary <- mitigation_id
  component <- 'mitigations' # Set up the project
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary)) # Set up the query parameters
  plural <- FALSE
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Test Plans
#'
#' Only applicable to test plans with Internal Control types
#'
#' @description "A test plan is a document that details how controls are
#'   assessed. Test plans identify the testing method or type of evidence
#'   obtained, specify the total sample size (split amongst testing rounds), and
#'   illustrate test steps or attributes."
#'
#' @details possible fields: testing_round_number, not_applicable, sample_size,
#'   testing_results, testing_conclusion, created_at, updated_at, control,
#'   assigned_user
#'
#' @inheritParams get_project
#'
#' @param control_test_plan_id Will get only one.
#'
#' @export
#' @return A tibble of control test plans
get_project_control_test_plans <- function(apikey, org, datacenter, control_test_plan_id, fields = NULL, pagesize = 50){
  
  primary <- control_test_plan_id
  component <- 'control_test_plans' # Set up the project
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary)) # Set up the query parameters
  plural <- FALSE
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Control Tests
#' 
#' Only applicable to test plans with Internal Control types
#'
#' @description "Control tests evaluate the operating effectiveness of a control."
#'
#' @inheritParams get_project
#'
#' @param control_test_id Will get only one.
#'
#' @export
#' @return A tibble of control tests
get_project_control_tests <- function(apikey, org, datacenter, control_test_id, fields = NULL, pagesize = 50){
  
  primary <- control_test_id
  component <- 'control_tests' # Set up the project
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary)) # Set up the query parameters
  plural <- FALSE
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Walkthroughs / Execute Procedures
#'
#' @description "A walkthrough is a series of steps you perform to establish the
#'   reliability of controls and test the design of controls. Each control you
#'   define has a corresponding walkthrough that is used to verify that the
#'   control is designed appropriately. In a Workplan workflow project, a
#'   walkthrough is called an execute procedure."
#'
#' @inheritParams get_project
#'
#' @param walkthrough_id Will get only one.
#'
#' @export
#' @return A tibble of walkthroughs
get_project_walkthrough <- function(apikey, org, datacenter, walkthrough_id, fields = NULL, pagesize = 50){
  
  primary <- walkthrough_id
  component <- 'walkthroughs' # Set up the project
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary)) # Set up the query parameters
  plural <- FALSE
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Issues
#'
#' @description "An issue is a problem, control gap, or exception that has been
#'   identified within a project. Adding an issue involves recording basic
#'   information about the issue and assigning the issue to an owner. Issues may
#'   also be known as deficiencies, findings, or another customized term."
#'
#' @details possible fields: title, description, creator_name, created_at,
#'   updated_at, position, owner, recommendation, deficiency_type, severity,
#'   published, identified_at, reference, reference_prefix, risk, scope,
#'   escalation, cause, effect, cost_impact, executive_summary, executive_owner,
#'   project_owner, closed, remediation_status, remediation_plan,
#'   remediation_date, actual_remediation_date, retest_deadline_date,
#'   actual_retest_date, retesting_results_overview, custom_attributes, project,
#'   entities
#'
#' @inheritParams get_project
#'
#' @param project_id Required if other parameter is blank. May obtain multiple rows.
#' @param issue_id Required if other parameter is blank. Will get only one.
#' @export
#' @return A tibble of issues
get_project_issue <- function(apikey, org, datacenter, project_id = NULL, issue_id = NULL, fields = NULL, pagesize = 50){
  
  primary <- project_id
  secondary <- issue_id
  component <- 'issues' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Actions
#'
#' @description "An action is a specific follow-up measure that is associated
#'   with an identified issue. You can add actions and assign action owners. You
#'   can also set up reminders for yourself to retest issues or track hours
#'   spent on retesting by self-assigning actions."
#'
#' @details possible fields: title, created_at, updated_at, owner_name,
#'   owner_email, send_recurring_reminder, include_issue_details,
#'   include_remediation_details, description, due_date, priority, closed,
#'   completed_date, status, submitted_on, custom_attributes, issue,
#'   assigned_by, cc_users
#'
#' @inheritParams get_project
#'
#' @param issue_id Required if other parameter is blank. May obtain multiple rows.
#' @param action_id Required if other parameter is blank. Will get only one.
#' @export
#' @return A tibble of actions
get_project_actions <- function(apikey, org, datacenter, issue_id = NULL, action_id = NULL, fields = NULL, pagesize = 50){
  
  primary <- issue_id
  secondary <- action_id
  component <- 'actions' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Request Items
#'
#' @description Capture what you asked for
#'
#' @inheritParams get_project
#'
#' @param request_id Will get only one.
#'
#' @export
#' @return A tibble of walkthroughs
get_project_request_items <- function(apikey, org, datacenter, request_id, fields = NULL, pagesize = 50){
  
  primary <- request_id
  component <- 'request_items' # Set up the project
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary)) # Set up the query parameters
  plural <- FALSE
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Projects - Entities
#'
#' @description Get all the entities
#'
#' @details Fields allowed: title, description, created_at, updated_at, parent, children_count, entity_category
#' 
#' @inheritParams get_project
#'
#' @param entity_id \code{NULL} will default to all entities.
#'
#' @return A tibble of entities
#' @export
get_project_entity <- function(apikey, org, datacenter, entity_id = NULL, fields = NULL, pagesize = 50){
  
  primary <- entity_id
  component <- 'entities' # Set up the project
  
  url <- paste0(hb_url(org, datacenter), hb_url_project(component, primary)) # Set up the query parameters
  plural <- is.null(primary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(apikey, url, params, datacenter, component, plural) # Download the data
  
  return(data) 
}

hb_project_one_only <- function(id1, id2){
  # at least one ID must be specified, but not both:
  
  check1 <- is.null(id1) 
  check2 <- is.null(id2) 
  
  # If check1 is not empty (FALSE) and check2 is not empty (FALSE)
  if (check1 + check2 == 0) {stop("Only one id value may be specified.", call. = FALSE)}
  
  # If check1 is empty (TRUE) and check2 is empty (TRUE)
  if (check1 + check2 == 2) {stop("At least one id value must be specified.", call. = FALSE)}
  
  return(TRUE)
}

#hb_url_project <- function(component, project_id = NULL, objective_id = NULL, issue_id = NULL, id = NULL){
hb_url_project <- function(component, primary = NULL, secondary = NULL){
# Used to build the correct urls for Project information, depending on the type of data captured
# Future - perhaps put plural switch here?

  url <- NULL
  
  ### This needs to break out GET A and GET ALL 
  
  ### PART ONE - Substitution based on components. with no main area overriding the query
  
  # Project override as structure is mildly different  
  if (component == 'projects'){
      if (is.null(primary)) {
      url <- 'projects/' # Get all projects
    } else {
      url <- paste0('projects/', primary) # Get one specific project
    }
    
    return(url) # Early return
  }
  
  # Project Types override, different because the ID is different
  if (component == 'project_types'){
    if (is.null(primary)) {
      url <- 'project_types/' # Get all projects
    } else {
      url <- paste0('project_types/', primary) # Get one specific project type
    }
    
    return(url) # Early return
  }
  
  # These have no higher level component, but ID is always required
  if(component %in% c('control_test_plans', 'walkthroughs', 'control_tests', 'mitigations', 'request_items')){
    url <- paste0(component, '/', primary)
    
    return(url) # Early return
  }
  
  if(component %in% c('entities')){
    if (is.null(primary)) {
      url <- 'entities/' # Get all projects
    } else {
      url <- paste0('entities/', primary) # Get one specific project type
    }
    
    return(url) # Early return
  }
  
  # Error check to make sure enough information is specified to proceed into next stage
  
  if (is.null(primary) & is.null(secondary)) # If the project or objective isn't specified, then at least one of the components needs to be
  {
    stop("Primary and secondary IDs must be specified for the requested component")
  }
  
  ### PART TWO - Substitution based on components and main area
  
  # Project based components
  if(component %in% c('planning_files', 'results_files', 'objectives', 'issues')){
    if (is.null(secondary)){
      url <- paste0('projects/', primary, '/', component)
        } else {
      url <- paste0(component, '/', secondary)
        }
  }
  
  # Objective based components
  if(component %in% c('narratives', 'risks', 'controls')){
    if (is.null(secondary)){
      url <- paste0('objectives/', primary, '/', component)
    } else {
      url <- paste0(component, '/', secondary)
    }
  }
  
  # Issue based components
  if(component %in% c('actions')){
    if (is.null(secondary)){
      url <- paste0('issues/', primary, '/', component)
    } else {
      url <- paste0(component, '/', secondary)
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
  
  # Make fields friendly for download
  if (length(fields) > 0) {
    fields <- paste0(fields, collapse = ',')
  }
  
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

  content_parts <- tibble(
    header = content_header,
    attributes = content_attributes
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
  
  # if(!(component %in% c('projects', 'planning_files'))){
  #   return(NULL)
  # }
  
  
  content_attributes_custom <- content_raw %>% 
    enter_object(attributes) %>% 
    enter_object(custom_attributes)
  
  if (nrow(content_attributes_custom) == 0){return (NULL)} # Check for blanks before going
  
  content_attributes_custom <- content_attributes_custom %>%
    gather_array() %>%
    gather_object() %>% 
    append_values_string("value") %>% # This is the reason why an early return is not needed
    select(.data$document.id, .data$array.index, .data$name, .data$value) %>% #propose to remove array.index
    as_tibble() %>%
    nest(custom_attributes = c(.data$array.index, .data$name, .data$value)) # CONSIDER RESHAPING TO HAVE ID, NAME, VALUE
  
  if (nrow(content_attributes_custom) == 0){return (NULL)}
  
  return(content_attributes_custom)
}

#' @importFrom rlang .data
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyjson spread_values enter_object gather_object gather_array spread_all append_values_string
hb_prj_parse_tags <- function(component, content_raw){
  # This only processes tags
  
  # if(!(component %in% c('projects'))){
  #   return(NULL)
  # }
  
  content_tags <- content_raw %>% 
    tidyjson::enter_object(attributes) %>% 
    tidyjson::enter_object(tag_list)
  
  if (nrow(content_tags) == 0){return (NULL)} # Check for blanks before going
  
  content_tags <- content_tags %>%
    gather_array() %>% 
    append_values_string() %>%
    select(.data$document.id, .data$array.index, tag_list = .data$string) %>% #propose to remove array.index
    as_tibble() %>%
    nest(tag_list = c(.data$array.index, .data$tag_list))
  
  if (nrow(content_tags) == 0){return (NULL)}
  
  return(content_tags)
}

#' @importFrom rlang .data
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyjson spread_values enter_object gather_object gather_array spread_all append_values_string
hb_prj_parse_rel <- function(component, content_raw){
  # This only processes tags

  # if(!(component %in% c('projects', 'planning_files'))){
  #   return(NULL)
  # }
  
  content_rel <- content_raw %>%
    tidyjson::enter_object(relationships) 
  
  if (nrow(content_rel) == 0){return (NULL)} # This occurs here because select statement will fail if these columsn doen't exist
  
  content_rel <- content_rel %>%
    gather_object %>%
    spread_all %>%
    select(.data$document.id, .data$name, relationship_id = .data$data.id, relationship_type = .data$data.type) %>% # ARE THESE SHOWING UP DIFFERENTLY AFTER SPREADING FOR DIFERENT TYPES?
    as_tibble() %>%
    nest(relationships = c(.data$name, relationship_id, relationship_type))
  
  return(content_rel)
}

#' @importFrom rlang .data
#' @importFrom dplyr select left_join
hb_prj_coljoin_data <- function(component, core, custom, tags, relationships){
  . <- NULL
  
  # Combine all the data together
  
  joined <- core$header %>%
    left_join(core$attributes, by = 'document.id') %>% {
        if(!is.null(custom)){
        left_join(., custom, by = 'document.id')
      } else . } %>% {
        if(!is.null(tags)){
        left_join(., tags, by = 'document.id')
      } else .} %>% {
        if(!is.null(relationships)){
          left_join(., relationships, by = 'document.id')
      } else .} %>%
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
  
  if (length(content_data) == 0){
    warning(paste('Downloaded json is blank. Is', component, 'empty?'))
    return(NULL)
  }

  core <- hb_prj_parse_standard(content_data) # Returns the three primary tables in all - header, attributes, relationships

  # Custom fields get - relevant to most except...
  custom <- hb_prj_parse_custom(component, content_data)
  tags <- hb_prj_parse_tags(component, content_data)
  relationships <- hb_prj_parse_rel(component, content_data)
  
  # First page, finished
  combined_data <- hb_prj_coljoin_data(component, core, custom, tags, relationships)
  
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
    relationships <- hb_prj_parse_rel(component, content_data)
    
    # Next page, finished
    combined_data_next <- hb_prj_coljoin_data(component, core, custom, tags, relationships)
    
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
