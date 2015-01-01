test_that("Validate GetScheduledSpike using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  ss <- GetScheduledSpike("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(ss, "character")
  
})
