#' @details The QueueDataWarehouse function allows to access to Data WareHouse data
#' and returns either json or sends a csv to a ftp server.
#'
#' Because of the Reporting API structure, this function requests the
#' report, then, if \code{enqueueOnly=FALSE}, checks the reporting queue to see if the report is completed,
#' and when the report returns as "done" pulls the report from the API (if ftp is not defined).
#' This checking process will occur up to the specified number of times (default 120),
#' with a delay between status checks (default 10 seconds). If the report does not
#' return as "done" or a "delivery_complete" after the number of tries have completed, the function will return
#' an error message. When \code{enqueueOnly=TRUE} and no ftp server is set, the report can be retrieved with Report.Get
#' using the reportId returned by the QueueDataWarehouse function.
#'
#' API limitations:
#' https://marketing.adobe.com/developer/documentation/data-warehouse/r-report-2
#' 
#' A single segment is supported. Multiple segments are not supported.
#' 
#' The following element properties are not supported in Data Warehouse reports:
#'     - selected
#'     - search
#'     - top
#'     - startingWith
#'     - sortBy
#'     
#' Calculated metrics are not supported.
#' 
#' Results for data warehouse reports can be accessed in two ways: directly through the API and
#' through FTP delivery. Email delivery is not supported.
#' 
#' All data warehouse results are paged in chunks of 20 MB. Add "page": to \code{Report.Get} 
#' to determine the page returned. If no page is specified then the first page is returned.
#'
#' @description A QueueDataWarehouse report is a report where metrics are
#' retrieved, broken down by an unlimited number of elements such as page, eVar, prop, etc, and
#' with or without temporal aggregation. Due API limitations, only one segment can be used if needed. 
#'
#' @title Queue a DataWarehouse Report
#'
#' @param reportsuite.id Report suite id
#' @param date.from Start date for the report (YYYY-MM-DD)
#' @param date.to End date for the report (YYYY-MM-DD)
#' @param metrics List of metrics to include in the report
#' @param elements List of elements to include in the report
#' @param date.granularity Time granularity of the report (year/month/week/day/hour), default to 'day'
#' @param segment.id Id of Adobe Analytics segment to retrieve the report for
#' @param data.current TRUE or FALSE - whether to include current data for reports that include today's date
#' @param expedite Set to TRUE to expedite the processing of this report
#' @param interval.seconds How long to wait between attempts
#' @param max.attempts Number of API attempts before stopping
#' @param validate whether to submit report definition for validation before requesting the data.
#' @param enqueueOnly only enqueue the report, don't get the data. returns report id, which you can later use to get the data
#' @param ftp FTP client parameters, only used if enqueueOnly=TRUE. Double check ftp parameters before requesting
#' a long report.
#'
#' @importFrom jsonlite toJSON unbox
#' @importFrom utils read.csv
#'
#' @return Data frame or report id, if enqueueOnly is TRUE
#'
#' @examples
#' \dontrun{
#' report.data <- QueueDataWarehouse("your_report_suite",
#'                             "2014-01-01",
#'                             "2014-01-07",
#'                             c("visits", "pageviews","event10"),
#'                             c("page","geoCountry","geoCity"),
#'                             enqueueOnly=TRUE,
#'                             ftp = list(host = "myftpserver.com",
#'                                        port = "21",
#'                                        directory = "/fromDW/",
#'                                        username = "memyselfandirene",
#'                                        password = "valkilmer",
#'                                        filename = "myreport.csv")
#'                             )
#'}
#' @export
#' 
QueueDataWarehouse <- function(reportsuite.id, date.from, date.to, metrics, elements,
                               date.granularity='day', segment.id='', data.current=TRUE,
                               expedite=FALSE, interval.seconds=5, max.attempts=120,
                               validate=TRUE, enqueueOnly=TRUE, ftp='') {
  
  if(enqueueOnly == TRUE && ftp == '') {
    stop("FTP credentials need to be specified when enqueueOnly = TRUE")
  }
  
  #RZ: move this out of public function interface
  format <- 'csv'
  
  # build JSON description
  # we have to use unbox to force jsonlite not put strings into single-element arrays
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- unbox(date.from)
  report.description$reportDescription$dateTo <- unbox(date.to)
  report.description$reportDescription$reportSuiteID <- unbox(reportsuite.id)
  report.description$reportDescription$dateGranularity <- unbox(date.granularity)
  
  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  report.description$reportDescription$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  report.description$reportDescription$elementDataEncoding <- unbox("utf8")
  
  
  #If segment is null, apply the standard segment unbox function
  #report.description$reportDescription$segments <- unbox(segment.id)
  
  if(as.list(segment.id)[1]==''){
    #report.description$reportDescription$segment_id <- unbox(segment.id)
    report.description$reportDescription$segments <- unbox(segment.id)
  }
  #If segment is not null, treat it like a list of metrics.
  else{
    report.description$reportDescription$segments <- data.frame( id = segment.id)
    
  }
  
  if(expedite==TRUE) {
    report.description$reportDescription$expedite <- unbox(expedite)
  }
  report.description$reportDescription$metrics = data.frame(id = metrics)
  
  report.description$reportDescription$metrics = data.frame(id = metrics)
  report.description$reportDescription$elements <- data.frame(id = elements)
  
  report.description$reportDescription$source <- unbox("warehouse")
  
  if(enqueueOnly==TRUE){
    report.description$reportDescription$ftp <- unbox(data.frame(ftp))
  }
  
  #RZ: Override enqueueOnly here so that report id always returned
  #Then, based on what user actually passed, determine whether it was an FTP report or console
  report.id <- SubmitJsonQueueReport(toJSON(report.description),interval.seconds=interval.seconds,max.attempts=max.attempts,validate=validate,enqueueOnly=TRUE,format=format)
  
  #This hack pages over results until an error occurs, which Adobe signals as end of results
  if(enqueueOnly==FALSE){
    result <- data.frame()
    counter <- 1
    keepgoing <- TRUE
    while (keepgoing){
      temp <- tryCatch(
        GetReport(report.id, 
                  interval.seconds = interval.seconds,
                  max.attempts = max.attempts,
                  format = format, 
                  print.attempts = TRUE,
                  page = counter), 
        error = function(w) data.frame()
      )
      if(nrow(temp) > 0){
        result <- rbind.fill(result, temp)
        rm(temp)
        counter <- counter + 1
      } else {
        keepgoing <- FALSE
      }
    }
    return(result)
  }
  
  return(report.id)
  
}
