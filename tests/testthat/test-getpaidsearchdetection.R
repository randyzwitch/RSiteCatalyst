test_that("Validate GetPaidSearchDetection using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  psd <- GetPaidSearchDetection("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(psd, "data.frame")
  
})
