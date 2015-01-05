test_that("Validate GetVersionAccess using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  va <- GetVersionAccess()
  
  #Validate returned value is a data.frame
  expect_is(va, "data.frame")
  
})
