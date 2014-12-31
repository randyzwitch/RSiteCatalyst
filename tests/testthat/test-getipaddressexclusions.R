test_that("Validate GetIPAddressExclusions using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  ip <- GetIPAddressExclusions("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(ip, "data.frame")
  
})
