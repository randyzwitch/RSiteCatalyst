test_that("Validate GetRealTimeSettings using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  grt <- GetRealTimeSettings("zwitchdev")
  grt1 <- GetRealTimeSettings(c("zwitchdev"))
  grt2 <- GetRealTimeSettings(c("zwitchdev", "zwitchjulia"))
  
  #Validate returned value is a data.frame
  expect_is(grt, "data.frame")
  expect_is(grt1, "data.frame")
  expect_is(grt2, "data.frame")
  
})
