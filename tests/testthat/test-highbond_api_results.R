upload <- data.frame(field_one = c('A','B','C'),
                     field_two = c(1, 2, 3),
                     field_three = c(TRUE, FALSE, TRUE),
                     field_four = c(as.Date('2019-01-01'), as.Date('2020-01-01'), as.Date('2021-12-31')),
                     field_five = c(as.POSIXct(Sys.time()), as.POSIXct(Sys.time()), as.POSIXct(Sys.time())),
                     field_six =  c(as.POSIXlt(Sys.time()), as.POSIXlt(Sys.time()), as.POSIXlt(Sys.time())))

test_that("Highbond Results - POST with PURGE", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  highbond_table <- Sys.getenv('highbond_table')
  
  # Check if upload worked
  
  expect_null(post_highbond_results(highbond_openapi, highbond_org, highbond_datacenter, highbond_table, upload = upload, purge = TRUE))
  
  # Col type check for re-download
  
  Sys.sleep(10) # Allow Highbond to process...
  
  download <- get_highbond_results(highbond_openapi, highbond_org, highbond_datacenter, highbond_table, timezone = 'Canada/Pacific')
  
  coltypes <- download$content$columns %>%
    dplyr::filter(field_name %in% c('field_one', 'field_two', 'field_three', 'field_four', 'field_five', 'field_six')) %>%
    dplyr::mutate(correctType = dplyr::case_when(
      field_name == 'field_one' & data_type == 'character'~ TRUE,
      field_name == 'field_two' & data_type == 'numeric'~ TRUE,
      field_name == 'field_three' & data_type == 'logical'~ TRUE,
      field_name == 'field_four' & data_type == 'date'~ TRUE,
      field_name == 'field_five' & data_type == 'datetime'~ TRUE,
      field_name == 'field_six' & data_type == 'datetime'~ TRUE,
      TRUE ~ FALSE
    ))
  
  expect_equal(sum(coltypes$correctType), ncol(upload))
})

test_that("Highbond Results - POST without Purge", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  highbond_table <- Sys.getenv('highbond_table')
  
  download <- get_highbond_results(highbond_openapi, highbond_org, highbond_datacenter, highbond_table)
  
  current_count <- nrow(download$content$data)
  
  expect_equal(post_highbond_results(highbond_openapi, highbond_org, highbond_datacenter, highbond_table, upload = upload, purge = FALSE), NULL)
  
  # Wait delay
  
  Sys.sleep(20)
  
  download <- get_highbond_results(highbond_openapi, highbond_org, highbond_datacenter, highbond_table)
  
  new_count <- nrow(download$content$data)
  
  expect_equal(current_count + nrow(upload), new_count)
})

test_that("Highbond Results - GET", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  highbond_table <- Sys.getenv('highbond_table')
  
  download <- get_highbond_results(highbond_openapi, highbond_org, highbond_datacenter, highbond_table)
  
  expect_true(nrow(download$content$data) >  0)
})

test_that("Highbond Results - POST - Stress test", {
  highbond_openapi <- Sys.getenv('highbond_openapi')
  highbond_org <- Sys.getenv('highbond_org')
  highbond_datacenter <- Sys.getenv('highbond_datacenter')
  highbond_table <- Sys.getenv('highbond_table')
  
  massupload <- do.call("rbind", replicate(1000, upload, simplify = FALSE))
  
  expect_null(post_highbond_results(highbond_openapi, highbond_org, highbond_datacenter, highbond_table, upload = massupload, purge = TRUE))
  
  Sys.sleep(10)
})
