test_that("Validate GetProps using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  props <- GetProps("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(props, "data.frame")
  
})
