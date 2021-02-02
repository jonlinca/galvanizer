check_api <- function(){
  key <- Sys.getenv('highbond_openapi') 
  if (!nzchar(key)){
    skip('API not available')
  }
}

test_that("Highbond Projects - GET projects (many)", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  highbond_fields <- c('name', 'state', 'status', 'created_at', 'updated_at', 'description', 'background', 'budget', 'position', 'certification', 'control_performance', 'risk_assurance', 'management_response', 'max_sample_size', 'number_of_testing_rounds', 'opinion', 'opinion_description', 'purpose', 'scope', 'start_date', 'target_date', 'tag_list', 'project_type', 'entities', 'collaborators')
  
  projects <- get_projects(hb_creds, project_id = NULL, pagesize = 2, fields = highbond_fields)
  
  expect_true(nrow(projects) > 1)
})

test_that("Highbond Projects - Relationships exist for more than first project", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  highbond_fields <- c('name', 'state', 'status', 'created_at', 'updated_at', 'description', 'background', 'budget', 'position', 'certification', 'control_performance', 'risk_assurance', 'management_response', 'max_sample_size', 'number_of_testing_rounds', 'opinion', 'opinion_description', 'purpose', 'scope', 'start_date', 'target_date', 'tag_list', 'project_type', 'entities', 'collaborators')
  
  projects <- get_projects(hb_creds, project_id = NULL, pagesize = 2, fields = highbond_fields)
  
  no_relationships <- length(which(sapply(projects$relationships, is.null)))

  expect_true(no_relationships == 0)
})

test_that("Highbond Projects - GET projects (one)", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  project_id <- 121339
  projects <- get_projects(hb_creds, project_id = project_id)
  
  expect_true(nrow(projects) == 1)
})

test_that("Highbond Projects - GET project types", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_types(hb_creds, NULL)
  b <- get_project_types(hb_creds, 183697, fields = c('name', 'description', 'workflow')) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET project planning files", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_planning_files(hb_creds, project_id = 121339) 
  b <- get_project_planning_files(hb_creds, planning_file_id = 211086) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET project result files", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_result_files(hb_creds, project_id = 121339)
  b <- get_project_result_files(hb_creds, results_file_id = 211799) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET objectives", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_objectives(hb_creds, project_id = 121341)
  b <- get_project_objectives(hb_creds, objective_id = 593214) 
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET narratives", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_narratives(hb_creds, objective_id = 593214) 
  b <- get_project_narratives(hb_creds, narrative_id = 131413) 
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET risks", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_risks(hb_creds, objective_id = 593214) 
  b <- get_project_risks(hb_creds, risk_id = 3684685) 
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET controls", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_controls(hb_creds, objective_id = 593214) 
  b <- get_project_controls(hb_creds, control_id = 4679764) 
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - Controls - Check Arrays", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  fields <- c('title', 'description', 'control_id', 'owner', 'frequency', 'control_type', 'prevent_detect', 'method', 
                       'status', 'position', 'created_at', 'updated_at', 'custom_attributes', 'objective', 'walkthrough', 'control_test_plan', 
                       'control_tests', 'mitigations', 'entities', 'owner_user')
  
  a <- get_project_controls(hb_creds, control_id = 4679764, fields = fields) 
  
  # control_tests, mitigations, and entities should be arrays
  
  a_count <- a$relationships[[1]] %>% 
    dplyr::filter(name == 'control_tests')
  
  expect_true(nrow(a_count) == 4)
})

test_that("Highbond Projects - GET control test plans", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_control_test_plans(hb_creds, control_test_plan_id = 4650972)
    
  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET control tests", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_control_tests(hb_creds, control_test_id = 3359066)
  
  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET walkthroughs", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_walkthroughs(hb_creds, walkthrough_id = 4218019)
    
  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET issues", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_issues(hb_creds, project_id = 121341)
  b <- get_project_issues(hb_creds, issue_id = 389481)
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET actions", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_actions(hb_creds, issue_id = 389481) 
  b <- get_project_actions(hb_creds, action_id = 126382)
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET request items", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_request_items(hb_creds, request_id = 1824393)

  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET entities", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_entities(hb_creds, NULL)
  b <- get_project_entities(hb_creds, entity_id = 69872)
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET entity categories", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_entity_categories(hb_creds, NULL)
  b <- get_project_entity_categories(hb_creds, entity_category_id = 7058)
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET Project Type Custom Attributes", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  project_id <- 121339
  
  a <- get_project_type_custom_attributes(hb_creds, project_type_id = 183697)
  b <- get_project_type_custom_attributes(hb_creds, custom_attributes_id = 21192)
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})


test_that("Highbond Projects - GET collaborators", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_collaborators(hb_creds, project_id = 121339)
  b <- get_project_collaborators(hb_creds, encoded_uid = 'MTIxMzM5OmUySnNpX003Mk5WU3BFbnhQTlN6')

  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})