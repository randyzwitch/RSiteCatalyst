test_that("Validate GetFeed using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  gf <- GetFeed("1109801")
  
  #Validate returned value is a data.frame
  expect_is(gf, "data.frame")
  
})
