test_that("Validate GetTimeStampEnabled using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  gtse <- GetTimeStampEnabled(c("zwitchdev", "zwitchjulia"))
  
  #Validate returned value is a data.frame
  expect_is(gtse, "data.frame")
  
})
