test_that("Validate GetMetrics using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  metrics <- GetMetrics("zwitchdev", report.type='warehouse')
  
  #Validate returned value is a data.frame
  expect_is(metrics, "data.frame")
  
})
