test_that("Validate GetKeyVisitors using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  kv <- GetKeyVisitors("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(kv, "data.frame")
  
})
