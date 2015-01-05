test_that("Validate QueueOvertime using legacy credentials", {
  
  skip_on_cran()
  
  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))
  
  #Single Metric, No granularity (summary report)
  aa <- QueueOvertime("zwitchdev",
                      "2014-01-01",
                      "2014-12-31",
                      "visits",
                      "")
  
  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")
  
  #Single Metric, Daily Granularity
  bb <- QueueOvertime("zwitchdev",
                      "2014-01-01",
                      "2014-12-31",
                      "visits",
                      "day")
  
  #Validate returned value is a data.frame
  expect_is(bb, "data.frame")
  
  #Single Metric, Week Granularity
  cc <- QueueOvertime("zwitchdev",
                      "2014-01-01",
                      "2014-12-31",
                      "visits",
                      "week")
  
  #Validate returned value is a data.frame
  expect_is(cc, "data.frame")
  
  #Two Metrics, Week Granularity
  dd <- QueueOvertime("zwitchdev",
                      "2014-01-01",
                      "2014-12-31",
                      c("visits", "pageviews"),
                      "week")
  
  #Validate returned value is a data.frame
  expect_is(dd, "data.frame")
  
  #Two Metrics, Month Granularity, Social Visitors
  ee <- QueueOvertime("zwitchdev",
                      "2014-01-01",
                      "2014-12-31",
                      c("visits", "pageviews"),
                      "month",
                      "5433e4e6e4b02df70be4ac63")
  
  #Validate returned value is a data.frame
  expect_is(ee, "data.frame")
  
  #Two Metrics, Day Granularity, Social Visitors, Anomaly Detection
  ff <- QueueOvertime("zwitchdev",
                      "2014-01-01",
                      "2014-12-31",
                      c("visits", "pageviews"),
                      "day",
                      "5433e4e6e4b02df70be4ac63",
                      anomaly.detection = "1")
  
  #Validate returned value is a data.frame
  expect_is(ff, "data.frame")
  
  
  
})
