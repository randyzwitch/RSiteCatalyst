test_that("Validate QueueFallout using legacy credentials", {

  skip_on_cran()

  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))

  aa <- QueueFallout("zwitchdev",
                     "2019-08-01",
                     "2019-12-31",
                     metric = "pageviews",
                     element = "page",
                     c("https://randyzwitch.com/creating-stacked-bar-chart-seaborn", "https://randyzwitch.com")
                     )


  #Validate returned value is a data.frame
  expect_is(aa, "data.frame")


  bb <- QueueFallout("zwitchdev",
                     "2019-08-01",
                     "2019-12-31",
                     metrics = "pageviews",
                     element = "page",
                     checkpoints = c("https://randyzwitch.com/creating-stacked-bar-chart-seaborn", "https://randyzwitch.com"),
                     segment.id = c("5433e4e6e4b02df70be4ac63"))


  #Validate returned value is a data.frame
  expect_is(bb, "data.frame")


  cc <- QueueFallout("zwitchdev",
                     "2019-08-01",
                     "2019-12-31",
                     metrics = "pageviews",
                     element = "page",
                     checkpoints = c("https://randyzwitch.com/creating-stacked-bar-chart-seaborn", "https://randyzwitch.com"),
                     segment.id = c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08"))

  #Validate returned value is a data.frame
  expect_is(cc, "data.frame")

  dd <- QueueFallout("zwitchdev",
                    "2019-08-01",
                    "2019-12-31",
                    metrics = "pageviews",
                    element = "page",
                    checkpoints = c("https://randyzwitch.com/creating-stacked-bar-chart-seaborn", "https://randyzwitch.com"),
                    segment.id = c("5433e4e6e4b02df70be4ac63","54adfe3de4b02df70be5ea08"),
                    enqueueOnly = TRUE
                    )

  #Validate returned value is numeric
  expect_is(dd, "numeric")

})
