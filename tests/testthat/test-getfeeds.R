test_that("Validate GetFeeds using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  gfs <- GetFeeds("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(gfs, "data.frame")
  
})
