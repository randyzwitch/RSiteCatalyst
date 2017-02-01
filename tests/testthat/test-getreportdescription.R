test_that("Validate GetReportDescription using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  grd <- GetReportDescription("15625765")
  
  #Validate returned value is a data.frame
  expect_is(grd, "data.frame")
  
})
