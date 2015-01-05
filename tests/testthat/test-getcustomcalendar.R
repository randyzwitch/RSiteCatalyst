test_that("Validate GetCustomCalendar using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  cal <- GetCustomCalendar("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(cal, "data.frame")
  
})
