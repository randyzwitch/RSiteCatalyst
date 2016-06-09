test_that("Validate GetReportsByIds using legacy credentials", {

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

bb <- QueueRanked("zwitchdev",
                       date.from = "2016-04-01",
                       date.to = "2016-04-20",
                       metrics = "pageviews",
                       elements = c("page", "sitesection", "browser") ,
                       segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08"),
                       enqueueOnly = TRUE
)



cc <- GetReportsByIds(list(aa,bb))

#Validate returned value is data.frame
expect_is(cc[[1]]$data.frame, "data.frame")

#Validate returned value is data.frame
expect_is(cc[[2]]$data.frame, "data.frame")

#Validate returned value is numeric
expect_is(cc[[1]]$report.id, "numeric")

#Validate returned value is numeric
expect_is(cc[[2]]$report.id, "numeric")


})
