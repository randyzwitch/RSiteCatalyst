test_that("Validate ViewProcessingRules using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  vpr <- ViewProcessingRules("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(vpr, "data.frame")
  
})
