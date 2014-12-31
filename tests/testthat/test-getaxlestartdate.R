test_that("Validate GetAxleStartDate using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  sd <- GetAxleStartDate("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(sd, "data.frame")
  
})
