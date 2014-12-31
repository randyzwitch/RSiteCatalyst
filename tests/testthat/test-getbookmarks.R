test_that("Validate GetBookmarks using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  url <- GetBookmarks()
  
  #Validate returned value is a data.frame
  expect_is(url, "data.frame")
  
})
