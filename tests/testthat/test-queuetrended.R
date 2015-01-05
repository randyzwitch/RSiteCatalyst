test_that("Validate QueueTrended using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  #Top 10 pages by pageviews, daily
  aa <- QueueTrended("zwitchdev",
                     "2014-12-01",
                     "2015-12-31",
                     date.granularity="day",
                     metrics="pageviews",
                     elements="page",
                     top="10")
  
  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")
  
  #Top 10 pages by pageviews, daily
  #Starting with 11
  bb <- QueueTrended("zwitchdev",
                     "2014-12-01",
                     "2015-12-31",
                     date.granularity="day",
                     metrics="pageviews",
                     elements="page",
                     top=10,
                     start=11)
  
  #Validate returned value is a data.frame
  expect_is(bb, "data.frame")
  
  #Top 7 pages by visits, weekly
  cc <- QueueTrended("zwitchdev",
                     "2014-12-01",
                     "2015-12-31",
                     date.granularity="week",
                     metrics="visits",
                     elements="page",
                     top="7")
  
  #Validate returned value is a data.frame
  expect_is(cc, "data.frame")
  
  #Selected Pages, Monthly
  dd <- QueueTrended("zwitchdev",
                     "2014-12-01",
                     "2015-12-31",
                     date.granularity="month",
                     metrics="exits",
                     elements="page",
                     selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"))
  
  #Validate returned value is a data.frame
  expect_is(dd, "data.frame")
  
  #Selected Pages, Quarterly, segment
  ee <- QueueTrended("zwitchdev",
                     "2014-12-01",
                     "2015-12-31",
                     date.granularity="quarter",
                     metrics="entries",
                     elements="page",
                     selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"),
                     segment.id = "5433e4e6e4b02df70be4ac63")
  
  #Validate returned value is a data.frame
  expect_is(ee, "data.frame")
  
  #Selected Pages, day, segment, Anomaly Detection
  ff <- QueueTrended("zwitchdev",
                     "2014-12-01",
                     "2015-12-31",
                     date.granularity="day",
                     metrics="pageviews",
                     elements="page",
                     selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"),
                     segment.id = "5433e4e6e4b02df70be4ac63",
                     anomaly.detection = "1")
  
  #Validate returned value is a data.frame
  expect_is(ff, "data.frame")
  
})





