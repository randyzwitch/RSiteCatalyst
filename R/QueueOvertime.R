#' @details Because of the Reporting API structure, this function first requests
#' the report, then checks the reporting queue to see if the report is completed,
#' and when the report returns as "done" pulls the report from the API. This checking process
#' will occur up to the specified number of times (default 120), with a delay between
#' status checks (default 5 seconds). If the report does not return as "done" after the 
#' number of tries have completed, the function will return an error message.
#'
#' @description A QueueOvertime report is a report where the only granularity allowed is time. This report allows for a single report suite, time granularity, 
#' multiple metrics, and a single segment. It is similar to the "Key Metrics" report or a Custom Event report 
#' within the Adobe Reports & Analytics interface. To get a summary report with no time granularity (i.e. a single row),
#' pass an empty string to the date.granularity function parameter.
#' 
#' @title Run an Overtime Report
#'
#' @param reportsuite.id Report suite id
#' @param date.from Start date for the report (YYYY-MM-DD)
#' @param date.to End date for the report (YYYY-MM-DD)
#' @param metrics List of metrics to include in the report
#' @param date.granularity Time granularity of the report (year/month/week/day/hour/''), default to 'day'
#' @param segment.id Id of Adobe Analytics segment to retrieve the report for
#' @param segment.inline Inline segment definition
#' @param anomaly.detection  Set to TRUE to include forecast data (only valid for day granularity with small date ranges)
#' @param data.current TRUE or FALSE - Whether to include current data for reports that include today's date
#' @param expedite Set to TRUE to expedite the processing of this report
#' @param interval.seconds How long to wait between attempts
#' @param max.attempts Number of API attempts before stopping
#' @param validate Weather to submit report definition for validation before requesting the data.
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return Data frame
#' 
#' @examples
#' \dontrun{
#' 
#' overtime1 <- QueueOvertime("your_report_suite",
#'                            date.from = "2014-04-01",
#'                            date.to = "2014-04-20",
#'                            metrics = c("pageviews", "visits", "bounces"),
#'                            date.granularity = "day")
#'                            
#' overtime2 <- QueueOvertime("your_report_suite",
#'                            date.from = "2014-04-01",
#'                            date.to = "2014-04-20",
#'                            metrics = c("pageviews", "visits", "bounces"),
#'                            date.granularity = "day",
#'                            segment.id = "5433e4e6e4b02df70be4ac63",
#'                            anomaly.detection = TRUE,
#'                            interval.seconds = 10,
#'                            max.attempts = 20)
#'                            
#' overtime3 <- QueueOvertime("your_report_suite",
#'                            date.from = "2014-04-01",
#'                            date.to = "2014-04-20",
#'                            metrics = c("pageviews", "visits", "bounces"),
#'                            date.granularity = "")
#' }
#'
#' @export

QueueOvertime <- function(reportsuite.id, date.from, date.to, metrics,
                        date.granularity='day', segment.id='', segment.inline='', anomaly.detection=FALSE,
                        data.current=FALSE, expedite=FALSE,interval.seconds=5,max.attempts=120,validate=TRUE) {
  
  # build JSON description
  # we have to use unbox to force jsonlist not put strings into single-element arrays
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- unbox(date.from)
  report.description$reportDescription$dateTo <- unbox(date.to)
  report.description$reportDescription$reportSuiteID <- unbox(reportsuite.id)
  report.description$reportDescription$dateGranularity <- unbox(date.granularity)
  report.description$reportDescription$segment_id <- unbox(segment.id)
  report.description$reportDescription$anomalyDetection <- unbox(anomaly.detection)
  report.description$reportDescription$currentData <- unbox(data.current)
  report.description$reportDescription$expedite <- unbox(expedite)
  if(segment.inline!="") {
    report.description$reportDescription$segments <- list(segment.inline)
  }
  report.description$reportDescription$metrics = data.frame(id = metrics)

  report.data <- SubmitJsonQueueReport(toJSON(report.description),interval.seconds=interval.seconds,max.attempts=max.attempts,validate=validate)

  return(report.data) 

}  
