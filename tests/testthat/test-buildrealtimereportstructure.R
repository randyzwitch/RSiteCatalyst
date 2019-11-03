test_that("Validate BuildRealTimeReportStructure using legacy credentials", {



  aa <- BuildRealTimeReportStructure(report.name = "test123",
                                     metric = "instances",
                                     elements = c("prop2", "searchenginekeyword", "geocountry"))


  #Validate returned value is a data.frame
  expect_is(aa, "list")

})





