test_that("Validate GetSuccessEvents using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  se <- GetSuccessEvents("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(se, "data.frame")
  
})
