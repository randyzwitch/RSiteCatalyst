test_that("Validate GetEcommerce using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  ecom <- GetEcommerce("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(ecom, "data.frame")
  
})
