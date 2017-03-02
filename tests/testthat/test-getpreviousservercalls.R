test_that("Validate GetPreviousServerCalls using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  gpsc <- GetPreviousServerCalls("zwitchdev")
  gpsc2 <- GetPreviousServerCalls(c("zwitchdev"))
  gpsc3 <- GetPreviousServerCalls(c("zwitchdev", "zwitchjulia"))
  
  #Validate returned value is a data.frame
  expect_is(gpsc, "data.frame")
  expect_is(gpsc2, "data.frame")
  expect_is(gpsc3, "data.frame")
  
})
