test_that("Validate GetRealTimeSettings using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  grt <- GetRealTimeSettings("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(grt, "data.frame")
  
})
