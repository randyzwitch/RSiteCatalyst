test_that("Validate QueueFallout using legacy credentials", {

  skip_on_cran()

  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))

  aa <- QueueFallout("zwitchdev",
                     "2014-12-01",
                     "2015-12-31",
                     metric="pageviews",
                     element="page",
                     c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data")
  )

  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")

})

bb <-QueueFallout("zwitchdev",
                  "2014-12-01",
                  "2015-12-31",
                  metrics="pageviews",
                  element="page",
                  checkpoints=c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data")
                  segment.id=c("54e4e00be4b093ca5b709931"))

)

#Validate returned value is a data.frame
expect_is(bb, "data.frame")

})

cc <-QueueFallout("zwitchdev",
                  "2014-12-01",
                  "2015-12-31",
                  metrics="pageviews",
                  element="page",
                  checkpoints=c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data")
                  segment.id=c("54e4e00be4b093ca5b709931","54e62bd0e4b0619d25327e86"))

)

#Validate returned value is a data.frame
expect_is(cc, "data.frame")

})
