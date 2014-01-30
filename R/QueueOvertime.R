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
#' @param anomaly.dection  set to TRUE to include forecast data (only valid for day granularity with small date ranges)
#' @param data.current TRUE or FALSE - whether to include current data for reports that include today's date
#' @param expedite set to TRUE to expedite the processing of this report
#'
#' @return Flat data frame containing datetimes and metric values
#'
#' @export

QueueOvertime <- function(reportsuite.id, date.from, date.to, metrics,
                        date.granularity='day', segment.id='', segment.inline='', anomaly.detection=FALSE,
                        data.current=FALSE, expedite=FALSE) {
  
  # build JSON description
  # we have to use jsonlite:::as.scalar to force jsonlist not put strings into single-element arrays
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- jsonlite:::as.scalar(date.from)
  report.description$reportDescription$dateTo <- jsonlite:::as.scalar(date.to)
  report.description$reportDescription$reportSuiteID <- jsonlite:::as.scalar(reportsuite.id)
  report.description$reportDescription$dateGranularity <- jsonlite:::as.scalar(date.granularity)
  report.description$reportDescription$segment_id <- jsonlite:::as.scalar(segment.id)
  report.description$reportDescription$anomalyDetection <- jsonlite:::as.scalar(anomaly.detection)
  report.description$reportDescription$currentData <- jsonlite:::as.scalar(data.current)
  report.description$reportDescription$expedite <- jsonlite:::as.scalar(expedite)
  if(segment.inline!="") {
    report.description$reportDescription$segments <- list(segment.inline)
  }
  report.description$reportDescription$metrics = data.frame(id = metrics)

  report.data <- JsonQueueReport(toJSON(report.description))

  return(report.data) 

}  
