test_that("Validate BuildRealTimeReportStructure using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  aa <- BuildRealTimeReportStructure(report.name="test123",
                                     metric="instances",
                                     elements = c("prop2", "searchenginekeyword", "geocountry"))
  
  
  #Validate returned value is a data.frame
  expect_is(aa, "list")
  
})





