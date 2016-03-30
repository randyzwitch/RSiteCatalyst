test_that("Validate BuildClassificationValueSegment using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  aa <- BuildClassificationValueSegment("page",
                                        c("julia", "python", "hadoop"),
                                        "OR") 
  
  #Validate returned value is a data.frame
  expect_is(aa, "list")
  
})





