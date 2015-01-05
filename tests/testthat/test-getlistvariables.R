test_that("Validate GetListVariables using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  listvar <- GetListVariables("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(listvar, "data.frame")
  
})
