test_that("Validate GetClickMapReporting using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  d <- GetClickMapReporting("zwitchjulia")
  
  #Validate returned value is a data.frame
  expect_is(d, "data.frame")
  
  d1 <- GetClickMapReporting(c("zwitchjulia"))
  
  #Validate returned value is a data.frame
  expect_is(d1, "data.frame")
  
  d2 <- GetClickMapReporting(c("zwitchdev", "zwitchjulia"))
  
  #Validate returned value is a data.frame
  expect_is(d2, "data.frame")
  
})
