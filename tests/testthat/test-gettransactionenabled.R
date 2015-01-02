test_that("Validate GetTransactionEnabled using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  te <- GetTransactionEnabled("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(te, "data.frame")
  
})
