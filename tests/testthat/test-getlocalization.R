test_that("Validate GetLocalization using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  localize <- GetLocalization("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(localize, "data.frame")
  
})
