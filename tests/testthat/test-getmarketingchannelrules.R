test_that("Validate GetMarketingChannelRules using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  mcr <- GetMarketingChannelRules("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(mcr, "data.frame")
  
  mcr2 <- GetMarketingChannelRules(c("zwitchdev", "zwitchjulia"))
  
  #Validate returned value is a data.frame
  expect_is(mcr2, "data.frame")
  
})
