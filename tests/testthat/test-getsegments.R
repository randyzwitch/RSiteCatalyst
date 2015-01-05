test_that("Validate GetSegments using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  seg <- GetSegments("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(seg, "data.frame")
  
})
