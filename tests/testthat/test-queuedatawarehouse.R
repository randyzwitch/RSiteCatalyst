test_that("Validate QueueDataWarehouse using legacy credentials", {

  skip_on_cran()

  #Correct [masked] credentials
  SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", ""))

  #FTP test
  report.id <- QueueDataWarehouse("zwitchdev",
                                    "2016-11-01",
                                    "2016-11-07",
                                    c("visits", "pageviews"),
                                    c("page"),
                                    enqueueOnly=TRUE,
                                    ftp = list(host = Sys.getenv("FTP", ""),
                                               port = "21",
                                               directory = "/DWtest/",
                                               username = Sys.getenv("FTPUSER", ""),
                                               password = Sys.getenv("FTPPW", ""),
                                               filename = "myreport.csv")
  )

  #Validate returned value is numeric id
  expect_is(report.id, "numeric")
  
  #Return answer to console
  # qdw <- QueueDataWarehouse("zwitchdev",
  #                                   "2016-11-01",
  #                                   "2016-11-07",
  #                                   c("visits", "pageviews"),
  #                                   c("page"),
  #                                   enqueueOnly=FALSE
  # )
  # 
  # #Validate returned value is a data.frame
  # expect_is(qdw, "data.frame")
  
  # dwresult <- QueueDataWarehouse("zwitchdev",
  #                                "2014-01-01",
  #                                "2017-02-02",
  #                                c("visits", "pageviews"),
  #                                c("page", "browser"),
  #                                date.granularity = 'hour',
  #                                enqueueOnly=FALSE
  # )
  # 
  # expect_equal(nrow(dwresult), 303510)
  # 
  # dwresult2 <- QueueDataWarehouse("zwitchdev",
  #                                 "2014-01-01",
  #                                 "2017-02-02",
  #                                 c("visits", "pageviews"),
  #                                 c("page", "browser"),
  #                                 date.granularity = 'hour',
  #                                 segment.id = '54adfe3de4b02df70be5ea08',
  #                                 enqueueOnly=FALSE
  # )
  # 
  # expect_equal(nrow(dwresult2), 56898)

})
