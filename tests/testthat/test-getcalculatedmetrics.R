test_that("Validate GetCalculatedMetrics using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  cm <- GetCalculatedMetrics("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(cm, "data.frame")
  
})
