test_that("Validate GetQueue using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  gq <- GetQueue()
  
  #Validate returned value is a data.frame
  expect_is(gq, c("list", "data.frame"))
  
})
