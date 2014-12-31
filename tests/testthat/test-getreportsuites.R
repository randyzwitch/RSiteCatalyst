test_that("Validate GetReportSuites using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  rs <- GetReportSuites()
  
  #Validate returned value is a data.frame
  expect_is(rs, "data.frame")
  
})
