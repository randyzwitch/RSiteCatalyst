test_that("Validate GetDashboards using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  dash <- GetDashboards("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(dash, "data.frame")
  
})
