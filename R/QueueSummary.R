#' @details The QueueSummary function returns a data frame containing a metric summary for the time period
#' selected.
#' 
#' Because of the Reporting API structure, this function first requests the 
#' report, then checks the reporting queue to see if the report is completed, and
#' when the report returns as "done" pulls the report from the API. This checking 
#' process will occur up to the specified number of times (default 120), with a
#' delay between status checks (default 5 seconds). If the report does not return
#' as "done" after the number of tries have completed, the function will return an
#' error message.
#' 
#'
#' @description A QueueSummary report is a summary report of metrics for one or more report 
#' suites for a given time period. Time period can be specified as year only ("2015"), 
#' year-month ("2015-04") or year-month-day ("2015-04-20")
#' 
#' 
#' @title Run a Summary Report
#'
#' @param reportsuite.ids Report suite ids
#' @param date Time period for the report (see Description)
#' @param metrics List of metrics to include in the report
#' @param interval.seconds How long to wait between attempts
#' @param max.attempts Number of API attempts before stopping
#' @param validate Weather to submit report definition for validation before requesting the data.
#'
#' @importFrom jsonlite toJSON unbox
#' 
#'
#' @return Data frame
#' 
#' @examples
#' \dontrun{
#' 
#' aa <- QueueSummary("zwitchdev", "2015", c("pageviews", "visits"))
#' 
#' }
#'
#' @export

QueueSummary <- function(reportsuite.ids, date, metrics, interval.seconds=5, max.attempts=120,validate=TRUE) {
  
  # build JSON description
  # we have to use unbox to force jsonlist not put strings into single-element arrays
  # new release of jsonlite will let us use jsonlite::singleton() (function is actually exported)
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$date <- unbox(date)
  report.description$reportDescription$metrics <- data.frame(id = metrics)
  report.description$reportDescription$elements <- list(list(id = unbox("reportsuite"), selected=c(reportsuite.ids)))
  
  report.data <- SubmitJsonQueueReport(toJSON(report.description),interval.seconds=interval.seconds,max.attempts=max.attempts,validate=validate)
  
  return(report.data) 
  
}  
