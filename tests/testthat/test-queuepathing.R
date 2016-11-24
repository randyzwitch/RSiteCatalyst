test_that("Validate QueuePathing using legacy credentials", {

  skip_on_cran()

  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))

  aa <- QueuePathing("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     metric="pageviews",
                     element="page",
                     c("::anything::", "::anything::", "::anything::", "::exited::")
  )


  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")



  bb <- QueuePathing("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     metric="pageviews",
                     element="page",
                     c("::anything::", "::anything::", "::anything::", "::exited::"),
                     segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08")

  )


  #Validate returned value is a data.frame
  expect_is(bb, "data.frame")


  cc <- QueuePathing("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     metric="pageviews",
                     element="page",
                     c("::anything::", "::anything::", "::anything::", "::exited::"),
                     segment.id="5433e4e6e4b02df70be4ac63"
  )


  #Validate returned value is a data.frame
  expect_is(cc, "data.frame")

  dd <- QueuePathing("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     metric="pageviews",
                     element="page",
                     c("::anything::", "::anything::", "::anything::", "::exited::"),
                     segment.id="5433e4e6e4b02df70be4ac63",
                     enqueueOnly = TRUE
  )


  #Validate returned value is a data.frame
  expect_is(dd, "numeric")


})
