test_that("Validate GetTimeZone using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  tz <- GetTimeZone("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(tz, "data.frame")
  
})
