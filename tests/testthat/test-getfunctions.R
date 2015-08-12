test_that("Validate GetFunctions using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  aa <- GetFunctions()
  
  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")
  
})