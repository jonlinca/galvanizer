test_that("Highbond Projects - GET ALL projects", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')

  highbond_fields <- 'name,state,status,created_at,updated_at,description,tag_list,project_type,entities'
  
  projects <- get_project(highbond_openapi, highbond_org, highbond_datacenter)
})

test_that("Highbond Projects - GET ONE projects", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  
  highbond_fields <- 'name,state,status,created_at,updated_at,description,tag_list,project_type,entities'
  
  project_id <- 121339
  
  projects <- get_project(highbond_openapi, highbond_org, highbond_datacenter, project_id = project_id)
})