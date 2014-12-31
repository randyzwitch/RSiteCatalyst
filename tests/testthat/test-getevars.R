test_that("Validate GetEvars using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  evar <- GetEvars("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(evar, "data.frame")
  
})
