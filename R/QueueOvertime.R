#' QueueOvertime
#'
#' Helper function to run an OverTime Report
#'
#' @param reportsuite.id report suite id
#' @param date.from start date for the report (YYYY-MM-DD)
#' @param date.to end date for the report (YYYY-MM-DD)
#' @param metrics list of metrics to include in the report
#' @param date.granularity time granularity of the report (year/month/week/day/hour), default to 'day'
#' @param segment.id id of Adobe Analytics segment to retrieve the report for
#' @param segment.inline inline segment definition
#' @param anomaly.detection  set to TRUE to include forecast data (only valid for day granularity with small date ranges)
#' @param data.current TRUE or FALSE - whether to include current data for reports that include today's date
#' @param expedite set to TRUE to expedite the processing of this report
#' @param interval.seconds How long to wait between attempts
#' @param max.attempts Number of API attempts before stopping
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return Flat data frame containing datetimes and metric values
#'
#' @export

QueueOvertime <- function(reportsuite.id, date.from, date.to, metrics,
                        date.granularity='day', segment.id='', segment.inline='', anomaly.detection=FALSE,
                        data.current=FALSE, expedite=FALSE,interval.seconds=5,max.attempts=120) {
  
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

  report.data <- JsonQueueReport(toJSON(report.description),interval.seconds=interval.seconds,max.attempts=max.attempts)

  return(report.data) 

}  
