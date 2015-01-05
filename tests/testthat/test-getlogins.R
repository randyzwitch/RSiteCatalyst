test_that("Validate GetLogins using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  logins <- GetLogins()
  
  #Validate returned value is a data.frame
  expect_is(logins, "data.frame")
  
})
