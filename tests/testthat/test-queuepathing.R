test_that("Validate QueuePathing using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  aa <- QueuePathing("zwitchdev", 
                     "2014-12-01", 
                     "2015-12-31", 
                     metric="pageviews", 
                     element="page", 
                     c("::anything::", "::anything::", "::anything::", "::exited::")
  )
  
  
  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")
  
})





