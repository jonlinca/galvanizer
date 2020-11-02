test_that("Highbond Projects - GET projects (many)", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  highbond_fields <- 'name,state,status,created_at,updated_at,description,tag_list,project_type,entities'
  
  projects <- get_project(hb_creds, project_id = NULL)
  
  expect_true(nrow(projects) > 1)
})

test_that("Highbond Projects - GET projects (one)", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  highbond_fields <- 'name,state,status,created_at,updated_at,description,tag_list,project_type,entities'
  
  project_id <- 121339
  projects <- get_project(hb_creds, project_id = project_id)
  
  expect_true(nrow(projects) == 1)
})

test_that("Highbond Projects - GET project types", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_type(hb_creds, NULL)
  b <- get_project_type(hb_creds, 183697, fields = c('name', 'description', 'workflow')) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET project planning files", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_planning_file(hb_creds, project_id = 121339) 
  b <- get_project_planning_file(hb_creds, planning_file_id = 211086) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET project result files", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_result_file(hb_creds, project_id = 121339)
  b <- get_project_result_file(hb_creds, results_file_id = 211799) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET objectives", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_objective(hb_creds, project_id = 121341)
  b <- get_project_objective(hb_creds, objective_id = 593214) 
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET narratives", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_narrative(hb_creds, objective_id = 593214) 
  b <- get_project_narrative(hb_creds, narrative_id = 131413) 
    
    expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET risks", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_risk(hb_creds, objective_id = 593214) 
  b <- get_project_risk(hb_creds, risk_id = 3684685) 
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET control", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_control(hb_creds, objective_id = 593214) 
  b <- get_project_control(hb_creds, control_id = 4679764) 
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET control test plans", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_control_test_plan(hb_creds, control_test_plan_id = 4650972)
    
  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET control tests", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_control_test(hb_creds, control_test_id = 3359066)
  
  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET walkthroughs", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_walkthrough(hb_creds, walkthrough_id = 4218019)
    
  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET issues", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_issue(hb_creds, project_id = 121341)
  b <- get_project_issue(hb_creds, issue_id = 389481)
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET actions", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_actions(hb_creds, issue_id = 389481) 
  b <- get_project_actions(hb_creds, action_id = 126382)
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET request items", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_request_items(hb_creds, request_id = 1824393)

  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET entities", {
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_entity(hb_creds, NULL)
  b <- get_project_entity(hb_creds, entity_id = 69872)
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})