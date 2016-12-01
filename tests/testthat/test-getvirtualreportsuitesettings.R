test_that("Validate GetVirtualReportSuiteSettings using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  gvrss <- GetVirtualReportSuiteSettings("vrs_zwitch0_vrs1")
  gvrss2 <- GetVirtualReportSuiteSettings(c("vrs_zwitch0_zwitchdevvrs", "vrs_zwitch0_vrs1"))
  
  #Validate returned value is a data.frame
  expect_is(gvrss, "data.frame")
  expect_is(gvrss2, "data.frame")
  
})
