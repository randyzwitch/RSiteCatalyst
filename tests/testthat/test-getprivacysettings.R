test_that("Validate GetPrivacySettings using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  aa <- GetPrivacySettings(c("zwitchdev", "zwitchjulia"))
  
  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")
  
  bb <- GetPrivacySettings("zwitchdev")
  
  #Validate returned value is a data.frame
  expect_is(bb, "data.frame")
  
})
