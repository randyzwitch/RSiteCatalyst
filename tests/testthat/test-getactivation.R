test_that("Validate GetActivation using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  act <- GetActivation("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(act, "data.frame")
  
})
