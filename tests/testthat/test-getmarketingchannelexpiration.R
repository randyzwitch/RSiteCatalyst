test_that("Validate GetMarketingChannelExpiration using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  mce <- GetMarketingChannelExpiration("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(mce, "data.frame")
  
})
