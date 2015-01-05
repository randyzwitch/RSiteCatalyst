test_that("Validate GetGeoSegmentation using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  geoseg <- GetGeoSegmentation("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(geoseg, "data.frame")
  
})
