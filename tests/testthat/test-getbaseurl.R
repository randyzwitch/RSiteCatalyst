test_that("Validate GetBaseURL using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  url <- GetBaseURL("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(url, "data.frame")
  
})
