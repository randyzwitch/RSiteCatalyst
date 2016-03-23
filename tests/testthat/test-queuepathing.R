test_that("Validate QueuePathing using legacy credentials", {

  skip_on_cran()

  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))

  aa <- QueuePathing("zwitchdev",
                     "2014-12-01",
                     "2015-12-31",
                     metric="pageviews",
                     element="page",
                     c("::anything::", "::anything::", "::anything::", "::exited::")
  )


  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")



  bb <- QueuePathing("zwitchdev",
                     "2014-12-01",
                     "2015-12-31",
                     metric="pageviews",
                     element="page",
                     c("::anything::", "::anything::", "::anything::", "::exited::"),
                     segment.id=c("54e4e00be4b093ca5b709931","54e62bd0e4b0619d25327e86")

  )


  #Validate returned value is a data.frame
  expect_is(bb, "data.frame")


  cc <- QueuePathing("zwitchdev",
                     "2014-12-01",
                     "2015-12-31",
                     metric="pageviews",
                     element="page",
                     c("::anything::", "::anything::", "::anything::", "::exited::"),
                     segment.id="54e4e00be4b093ca5b709931"
  )


  #Validate returned value is a data.frame
  expect_is(cc, "data.frame")


})
