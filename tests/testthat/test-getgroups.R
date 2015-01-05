test_that("Validate GetGroups using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  groups <- GetGroups()
  
  #Validate returned value is a data.frame
  expect_is(groups, "data.frame")
  
})
