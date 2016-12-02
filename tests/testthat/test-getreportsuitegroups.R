test_that("Validate GetReportSuiteGroups using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  grsg <- GetReportSuiteGroups("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(grsg, "data.frame")
  
})
