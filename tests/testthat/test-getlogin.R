test_that("Validate GetLogin using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  login <- GetLogin("rzwitch")
  
  #Validate returned value is a data.frame
  expect_is(login, "data.frame")
  
})
