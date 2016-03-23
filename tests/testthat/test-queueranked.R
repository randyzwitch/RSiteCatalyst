test_that("Validate QueueRanked using legacy credentials", {

  skip_on_cran()

  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))

  #Top 50 pages
  aa <- QueueRanked("zwitchdev",
                    "2014-01-01",
                    "2014-12-31",
                    "pageviews",
                    "page",
                    top="50",
                    start="1")

  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")

  #Next 50 pages
  bb <- QueueRanked("zwitchdev",
                    "2014-01-01",
                    "2014-12-31",
                    "pageviews",
                    "page",
                    top="50",
                    start="51")

  #Validate returned value is a data.frame
  expect_is(bb, "data.frame")

  #Two specific pages
  #Visits and pageviews metric
  cc <- QueueRanked("zwitchdev",
                    "2014-01-01",
                    "2014-12-31",
                    c("visits", "pageviews"),
                    "page",
                    selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data")
  )

  #Validate returned value is a data.frame
  expect_is(cc, "data.frame")

  #Two specific pages - "Home Page" and "Search Results"
  #Visits and pageviews metric, Social Visitors, Current Data
  dd <- QueueRanked("zwitchdev",
                    "2015-01-01",
                    "2015-12-31",
                    c("visits", "pageviews"),
                    "page",
                    selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"),
                    segment.id = "5433e4e6e4b02df70be4ac63",
                    data.current = "1")

  #Validate returned value is a data.frame
  expect_is(dd, "data.frame")

  ee <- QueueRanked("zwitchdev",
                         date.from = "2015-04-01",
                         date.to = "2015-04-20",
                         metrics = "pageviews",
                         elements = c("page", "sitesection")
  )


  #Validate returned value is a data.frame
  expect_is(ee, "data.frame")

  ff <- QueueRanked("zwitchdev",
                         date.from = "2015-04-01",
                         date.to = "2015-04-20",
                         metrics = "pageviews",
                         elements = c("page", "sitesection", "browser")
  )


  #Validate returned value is a data.frame
  expect_is(ff, "data.frame")

  #Top 50 pages, 2 segments
  gg <- QueueRanked("zwitchdev",
                    "2014-01-01",
                    "2014-12-31",
                    "pageviews",
                    "page",
                    top="50",
                    start="1",
                    segment.id=c("54e4e00be4b093ca5b709931","54e62bd0e4b0619d25327e86"))

  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")

  #Next 50 pages, 2 segments
  hh <- QueueRanked("zwitchdev",
                    "2014-01-01",
                    "2014-12-31",
                    "pageviews",
                    "page",
                    top="50",
                    start="51",
                    segment.id=c("54e4e00be4b093ca5b709931","54e62bd0e4b0619d25327e86"))

  #Validate returned value is a data.frame
  expect_is(bb, "data.frame")

  #Two specific pages, 2 segments
  #Visits and pageviews metric
  ii <- QueueRanked("zwitchdev",
                    "2014-01-01",
                    "2014-12-31",
                    c("visits", "pageviews"),
                    "page",
                    selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"),
                    segment.id=c("54e4e00be4b093ca5b709931","54e62bd0e4b0619d25327e86")
  )

  #Validate returned value is a data.frame
  expect_is(cc, "data.frame")

  #Two specific pages - "Home Page" and "Search Results"
  #Visits and pageviews metric, 2 segments, Current Data
  jj <- QueueRanked("zwitchdev",
                    "2015-01-01",
                    "2015-12-31",
                    c("visits", "pageviews"),
                    "page",
                    selected = c("http://randyzwitch.com/r-amazon-ec2", "http://randyzwitch.com/julia-import-data"),
                    segment.id=c("54e4e00be4b093ca5b709931","54e62bd0e4b0619d25327e86"),
                    data.current = "1")

  #Validate returned value is a data.frame
  expect_is(dd, "data.frame")

  # 2 segments added to ee test
  kk <- QueueRanked("zwitchdev",
                         date.from = "2015-04-01",
                         date.to = "2015-04-20",
                         metrics = "pageviews",
                         elements = c("page", "sitesection") ,
                         segment.id=c("54e4e00be4b093ca5b709931","54e62bd0e4b0619d25327e86")
  )


  #Validate returned value is a data.frame
  expect_is(ee, "data.frame")

  #2 segments added to ff
  ll <- QueueRanked("zwitchdev",
                         date.from = "2015-04-01",
                         date.to = "2015-04-20",
                         metrics = "pageviews",
                         elements = c("page", "sitesection", "browser") ,
                         segment.id=c("54e4e00be4b093ca5b709931","54e62bd0e4b0619d25327e86")
  )


  #Validate returned value is a data.frame
  expect_is(ff, "data.frame")


})
