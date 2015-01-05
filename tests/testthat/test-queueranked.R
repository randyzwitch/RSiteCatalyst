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
  
})



