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
#' suites for a given time period. Time period in the date parameter can be specified as year only ("2015"),
#' year-month ("2015-04") or year-month-day ("2015-04-20"); alternatively, date.to and date.from
#' are available for custom date ranges.
#'
#'
#' @title Run a Summary Report
#'
#' @param reportsuite.ids Report suite ids
#' @param date Time period for the report (see Description)
#' @param metrics List of metrics to include in the report
#' @param interval.seconds How long to wait between attempts
#' @param max.attempts Number of API attempts before stopping
#' @param validate whether to submit report definition for validation before requesting the data.
#' @param date.from Start date for the report (YYYY-MM-DD)
#' @param date.to End date for the report (YYYY-MM-DD)
#' @param enqueueOnly only enqueue the report, don't get the data. returns report id, which you can later use to get the data
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
#' bb <- QueueSummary("zwitchdev", "", c("pageviews", "visits"), 
#'                    date.from = "2016-01-01", date.to="2016-01-15")
#' enqueued.reprot.id <- QueueSummary("zwitchdev", "", c("pageviews", "visits"), 
#'                    date.from = "2016-01-01", date.to="2016-01-15",
#'                    enqueueOnly=TRUE)
#' }
#'
#' @export

QueueSummary <- function(reportsuite.ids, date = "", metrics, interval.seconds=5, max.attempts=120,validate=TRUE, date.from = "", date.to = "",enqueueOnly=FALSE) {

  # build JSON description
  # we have to use unbox to force jsonlist not put strings into single-element arrays
  # new release of jsonlite will let us use jsonlite::singleton() (function is actually exported)
  
  if(date != "" && (date.from != "" || date.to != "")){
    stop("Use date OR date.from and date.to together, not both.")
  }
  
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$date <- unbox(date)
  report.description$reportDescription$dateTo <- unbox(date.to)
  report.description$reportDescription$dateFrom <- unbox(date.from)
  report.description$reportDescription$metrics <- data.frame(id = metrics)
  report.description$reportDescription$elements <- list(list(id = unbox("reportsuite"), selected=c(reportsuite.ids)))

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  report.description$reportDescription$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  report.description$reportDescription$elementDataEncoding <- unbox("utf8")

  report.data <- SubmitJsonQueueReport(toJSON(report.description),interval.seconds=interval.seconds,max.attempts=max.attempts,validate=validate,enqueueOnly=enqueueOnly)

  return(report.data)

}
