test_that("Validate GetDataWarehouseDisplay using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  dw <- GetDataWarehouseDisplay("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(dw, "data.frame")
  
})
