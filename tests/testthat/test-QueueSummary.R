test_that("Validate QueueSummary using legacy credentials", {

  skip_on_cran()

  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))

  aa <- QueueSummary(c("zwitchdev", "zwitchjulia"), "2016", c("visits", "pageviews"))

  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")

  bb <- QueueSummary(c("zwitchdev"), "2016", c("visits", "pageviews"))

  #Validate returned value is a data.frame
  expect_is(bb, "data.frame")

  cc <- QueueSummary("zwitchjulia", "2016", c("visits", "pageviews"))

  #Validate returned value is a data.frame
  expect_is(cc, "data.frame")

  dd <- QueueSummary("zwitchjulia", "2016", c("visits"))

  #Validate returned value is a data.frame
  expect_is(dd, "data.frame")

  ee <- QueueSummary("zwitchjulia", "2016", "visits")

  #Validate returned value is a data.frame
  expect_is(ee, "data.frame")

  ff <- QueueSummary("zwitchjulia", "2016-04", "visits")

  #Validate returned value is a data.frame
  expect_is(ff, "data.frame")

  gg <- QueueSummary("zwitchjulia", "2016-04-20", "visits")

  #Validate returned value is a data.frame
  expect_is(gg, "data.frame")

  hh <- QueueSummary("zwitchdev", "", c("pageviews", "visits"), date.from = "2016-01-01",
                     date.to="2016-01-15")

  #Validate returned value is a data.frame
  expect_is(hh, "data.frame")

  ii <- QueueSummary("zwitchdev", "", c("pageviews", "visits"), date.from = "2016-01-01",
                     date.to="2016-01-15", enqueueOnly=TRUE)

  #Validate returned value is a numeric
  expect_is(ii, "numeric")

})
