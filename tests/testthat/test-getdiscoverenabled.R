test_that("Validate GetDiscoverEnabled using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  disc <- GetDiscoverEnabled("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(disc, "data.frame")
  
})
