test_that("Validate GetClassifications using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  saint <- GetClassifications("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(saint, "data.frame")
  
})
