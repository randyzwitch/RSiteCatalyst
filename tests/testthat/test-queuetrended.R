test_that("Validate QueueTrended using legacy credentials", {

  skip_on_cran()

  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))

  #Top 10 pages by pageviews, daily
  aa <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="day",
                     metrics="pageviews",
                     elements="page",
                     top="10")

  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")

  #Top 10 pages by pageviews, daily
  #Starting with 11
  bb <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="day",
                     metrics="pageviews",
                     elements="page",
                     top=10,
                     start=11)

  #Validate returned value is a data.frame
  expect_is(bb, "data.frame")

  #Top 7 pages by visits, weekly
  cc <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="week",
                     metrics="visits",
                     elements="page",
                     top="7")

  #Validate returned value is a data.frame
  expect_is(cc, "data.frame")

  #Selected Pages, Monthly
  dd <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="month",
                     metrics="exits",
                     elements="page",
                     selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"))

  #Validate returned value is a data.frame
  expect_is(dd, "data.frame")

  #Selected Pages, Quarterly, segment
  ee <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="quarter",
                     metrics="entries",
                     elements="page",
                     selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"),
                     segment.id = "5433e4e6e4b02df70be4ac63")

  #Validate returned value is a data.frame
  expect_is(ee, "data.frame")

  #Selected Pages, day, segment, Anomaly Detection
  ff <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="day",
                     metrics="pageviews",
                     elements="page",
                     selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"),
                     segment.id = "5433e4e6e4b02df70be4ac63",
                     anomaly.detection = "1")

  #Validate returned value is a data.frame
  expect_is(ff, "data.frame")

  gg <- QueueTrended("zwitchdev",
                           date.from = "2016-04-01",
                           date.to = "2016-04-20",
                           metrics = c("pageviews","visits"),
                           elements = c("sitesection")
  )

  #Validate returned value is a data.frame
  expect_is(gg, "data.frame")

  hh <- QueueTrended("zwitchdev",
                           date.from = "2016-04-01",
                           date.to = "2016-04-20",
                           metrics = "pageviews",
                           elements = c("sitesection", "page")
  )

  #Validate returned value is a data.frame
  expect_is(hh, "data.frame")

  ii <- QueueTrended("zwitchdev",
                           date.from = "2016-04-01",
                           date.to = "2016-04-02",
                           metrics = "pageviews",
                           elements = c("sitesection", "page","browser")
  )

  #Validate returned value is a data.frame
  expect_is(ii, "data.frame")

  #Top 10 pages by pageviews, daily, 2 segments
  jj <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="day",
                     metrics="pageviews",
                     elements="page",
                     top="10",
                     segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08"))

  #Validate returned value is a data.frame
  expect_is(jj, "data.frame")

  #Top 10 pages by pageviews, daily, 2 segments
  #Starting with 11
  kk <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="day",
                     metrics="pageviews",
                     elements="page",
                     top=10,
                     start=11,
                     segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08"))

  #Validate returned value is a data.frame
  expect_is(kk, "data.frame")

  #Top 7 pages by visits, weekly, 2 segments
  ll <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="week",
                     metrics="visits",
                     elements="page",
                     top="7",
                     segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08"))

  #Validate returned value is a data.frame
  expect_is(ll, "data.frame")

  #Selected Pages, Monthly, 2 segments
  mm <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="month",
                     metrics="exits",
                     elements="page",
                     selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"),
                     segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08"))

  #Validate returned value is a data.frame
  expect_is(mm, "data.frame")

  #Selected Pages, Quarterly, 2 segments
  nn <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="quarter",
                     metrics="entries",
                     elements="page",
                     selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"),
                     segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08"))

  #Validate returned value is a data.frame
  expect_is(nn, "data.frame")

  #Selected Pages, day, segment, Anomaly Detection, 2 segments
  oo <- QueueTrended("zwitchdev",
                     "2016-08-01",
                     "2016-12-31",
                     date.granularity="day",
                     metrics="pageviews",
                     elements="page",
                     selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"),
                     segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08"),
                     anomaly.detection = "1")

  #Validate returned value is a data.frame
  expect_is(oo, "data.frame")

  #gg repeated with 2 segments
  pp <- QueueTrended("zwitchdev",
                           date.from = "2016-04-01",
                           date.to = "2016-04-20",
                           metrics = c("pageviews","visits"),
                           elements = c("sitesection") ,
                           segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08")
  )

  #Validate returned value is a data.frame
  expect_is(pp, "data.frame")

  #hh repeated with 2 segments
  qq <- QueueTrended("zwitchdev",
                           date.from = "2016-04-01",
                           date.to = "2016-04-20",
                           metrics = "pageviews",
                           elements = c("sitesection", "page") ,
                           segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08")
  )

  #Validate returned value is a data.frame
  expect_is(qq, "data.frame")

  #ii repeated with 2 segments
  rr <- QueueTrended("zwitchdev",
                           date.from = "2016-04-01",
                           date.to = "2016-04-02",
                           metrics = "pageviews",
                           elements = c("sitesection", "page","browser") ,
                           segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08")
  )

  #Validate returned value is a data.frame
  expect_is(rr, "data.frame")


  #check enqueueOnly returns numeric report id
  ss <- QueueTrended("zwitchdev",
                           date.from = "2016-04-01",
                           date.to = "2016-04-02",
                           metrics = "pageviews",
                           elements = c("sitesection", "page","browser") ,
                           segment.id=c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08"),
                           enqueueOnly=TRUE
  )

  #Validate returned value is a numeric
  expect_is(ss, "numeric")
  
  #GitHub issue 207, fixes Error in if (!is.null(elements[i, ]$classification) && nchar(elements[i,  : 
  #missing value where TRUE/FALSE needed 
  issue207 <- QueueTrended(
    reportsuite.id = "zwitchdev",
    date.from = "2016-02-01",
    date.to = "2016-02-01",
    top ="1000",
    metrics = c("visits"),
    elements = c("page", "mobiledevicetype"),
    classification = "JuliaPages",
    date.granularity = "week", 
    segment.id = "", 
    data.current = TRUE,
    expedite = FALSE, 
    interval.seconds = 5, 
    max.attempts = 10000,
    validate = TRUE, 
    enqueueOnly = FALSE
  )
  
  #Validate returned value is a data.frame
  expect_is(issue207, "data.frame")
  
})
