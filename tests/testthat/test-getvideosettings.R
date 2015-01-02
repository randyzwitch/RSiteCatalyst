test_that("Validate GetVideoSettings using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  vs <- GetVideoSettings("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(vs, "data.frame")
  
})
