test_that("Validate GetMobileAppReporting using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  ma <- GetMobileAppReporting("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(ma, "data.frame")
  
})
