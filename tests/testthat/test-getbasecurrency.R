test_that("Validate GetBaseCurrency using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  bc <- GetBaseCurrency("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(bc, "data.frame")
  
})
