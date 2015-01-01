test_that("Validate GetMarketingChannels using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  mc <- GetMarketingChannels("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(mc, "data.frame")
  
})
