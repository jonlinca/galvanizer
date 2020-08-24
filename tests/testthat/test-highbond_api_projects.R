test_that("Highbond Projects - GET projects (many)", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')

  highbond_fields <- 'name,state,status,created_at,updated_at,description,tag_list,project_type,entities'
  
  projects <- get_project(highbond_openapi, highbond_org, highbond_datacenter)
  
  expect_true(nrow(projects) > 1)
})

test_that("Highbond Projects - GET projects (one)", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  highbond_fields <- 'name,state,status,created_at,updated_at,description,tag_list,project_type,entities'
  
  project_id <- 121339
  projects <- get_project(highbond_openapi, highbond_org, highbond_datacenter, project_id = project_id)
  
  expect_true(nrow(projects) == 1)
})

test_that("Highbond Projects - GET project types", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_type(highbond_openapi, highbond_org, highbond_datacenter)
  b <- get_project_type(highbond_openapi, highbond_org, highbond_datacenter, 183697, fields = c('name', 'description', 'workflow')) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET project planning files", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_planning_file(highbond_openapi, highbond_org, highbond_datacenter, project_id = 121339) 
  b <- get_project_planning_file(highbond_openapi, highbond_org, highbond_datacenter, planning_file_id = 211086) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET project result files", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_results_file(highbond_openapi, highbond_org, highbond_datacenter, project_id = 121339)
  b <- get_project_results_file(highbond_openapi, highbond_org, highbond_datacenter, results_file_id = 211799) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET objectives", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_objective(highbond_openapi, highbond_org, highbond_datacenter, project_id = 121341)
  b <- get_project_objective(highbond_openapi, highbond_org, highbond_datacenter, objective_id = 593214) 
    
    expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET narratives", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_narrative(highbond_openapi, highbond_org, highbond_datacenter, objective_id = 593214) 
  b <- get_project_narrative(highbond_openapi, highbond_org, highbond_datacenter, narrative_id = 131413) 
    
    expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET risks", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_risk(highbond_openapi, highbond_org, highbond_datacenter, objective_id = 593214) 
  b <- get_project_risk(highbond_openapi, highbond_org, highbond_datacenter, risk_id = 3684685) 
    
    expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET control", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_control(highbond_openapi, highbond_org, highbond_datacenter, objective_id = 593214) 
  b <- get_project_control(highbond_openapi, highbond_org, highbond_datacenter, control_id = 4679764) 
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET control test plans", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_control_test_plans(highbond_openapi, highbond_org, highbond_datacenter, control_test_plan_id = 4650972)
    
  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET control tests", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_control_tests(highbond_openapi, highbond_org, highbond_datacenter, control_test_id = 3359066)
  
  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET walkthroughs", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_walkthrough(highbond_openapi, highbond_org, highbond_datacenter, walkthrough_id = 4218019)
    
  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET issues", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_issue(highbond_openapi, highbond_org, highbond_datacenter, project_id = 121341)
  b <- get_project_issue(highbond_openapi, highbond_org, highbond_datacenter, issue_id = 389481)
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET actions", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_actions(highbond_openapi, highbond_org, highbond_datacenter, issue_id = 389481) 
  b <- get_project_actions(highbond_openapi, highbond_org, highbond_datacenter, action_id = 126382)
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET request items", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_request_items(highbond_openapi, highbond_org, highbond_datacenter, request_id = 1824393)

  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET entities", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  a <- get_project_entity(highbond_openapi, highbond_org, highbond_datacenter, NULL)
  b <- get_project_entity(highbond_openapi, highbond_org, highbond_datacenter, entity_id = 69872)
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})