test_that("Validate GetReport using legacy credentials", {

  skip_on_cran()

  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))

aa <- QueueFallout("zwitchdev",
                  "2015-12-01",
                  "2016-12-31",
                  metrics="pageviews",
                  element="page",
                  checkpoints=c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"),
                  segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08"),
                  enqueueOnly=TRUE
                  )

bb <- GetReport(aa,interval.seconds=5,max.attempts=100)

#Validate returned value is data.frame
expect_is(bb, "data.frame")

})
