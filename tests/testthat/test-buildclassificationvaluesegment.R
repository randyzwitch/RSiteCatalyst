test_that("Validate BuildClassificationValueSegment using legacy credentials", {

  aa <- BuildClassificationValueSegment("page",
                                        c("julia", "python", "hadoop"),
                                        "OR")

  #Validate returned value is a data.frame
  expect_is(aa, "list")

})





