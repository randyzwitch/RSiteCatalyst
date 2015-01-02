test_that("Validate GetUniqueVisitorVariable using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  uv <- GetUniqueVisitorVariable("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(uv, "data.frame")
  
})
