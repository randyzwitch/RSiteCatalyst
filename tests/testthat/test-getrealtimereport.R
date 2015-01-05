test_that("Validate SaveRealTimeSettings using legacy credentials", {
  
  skip_on_cran()
  
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  #Create full suite of tests once Issue #76 fixed
  aa <- GetRealTimeReport("zwitchdev", "pageviews")
  
  #Check that returned value is a data.frame
  expect_is(aa, "data.frame")
  
})
