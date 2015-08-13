test_that("Validate GetTemplate using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  aa <- GetTemplate(c("zwitchdev", "zwitchjulia"))
  
  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")
  
  bb <- GetTemplate("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(bb, "data.frame")
  
})
