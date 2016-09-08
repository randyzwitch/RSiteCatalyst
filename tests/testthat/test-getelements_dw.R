test_that("Validate GetElements using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  elem <- GetElements("zwitchdev", report.type='warehouse')
  
  #Validate returned value is a data.frame
  expect_is(elem, "data.frame")
  
})
