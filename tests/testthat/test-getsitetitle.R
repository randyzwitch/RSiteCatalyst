test_that("Validate GetSiteTitle using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  st <- GetSiteTitle("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(st, "data.frame")
  
})
