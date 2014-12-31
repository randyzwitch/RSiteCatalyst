test_that("Validate GetInternalURLFilters using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  filters <- GetInternalURLFilters("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(filters, "data.frame")
  
})
