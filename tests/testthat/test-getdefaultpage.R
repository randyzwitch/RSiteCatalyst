test_that("Validate GetDefaultPage using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  dp <- GetDefaultPage("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(dp, "data.frame")
  
})
