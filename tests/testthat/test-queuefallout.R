test_that("Validate QueueFallout using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  aa <- QueueFallout("zwitchdev", 
                     "2014-12-01", 
                     "2015-12-31", 
                     metric="pageviews", 
                     element="page", 
                     c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data")
  )
  
  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")
  
})





