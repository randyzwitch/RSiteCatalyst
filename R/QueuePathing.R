#' @details Because of the Reporting API structure, this function first
#' requests the report, then checks the reporting queue to see if the report
#' is completed, and when the report returns as "done" pulls the report from the
#' API. This checking process will occur up to the specified number of times
#' (default 120), with a delay between status checks (default 5 seconds). If the
#' report does not return as "done" after the number of tries have completed, the
#' function will return an error message.
#'
#' @description A QueuePathing Report is a report that shows how often visitors
#' go from Page A to Page B to Page C on site.
#'
#' @title Run a Pathing report
#'
#' @param reportsuite.id Report suite id
#' @param date.from Start date for the report (YYYY-MM-DD)
#' @param date.to End date for the report (YYYY-MM-DD)
#' @param metric Single metric to include in the report (usually 'pageviews')
#' @param element Single pathed element (usually 'page')
#' @param pattern Character vector of items in the path (up to 3)
#' use "::anything::" as a wildcard. For example c("Home","::anything::","::anything::") will return all paths that start with the home page,
#' c("::anything::","Home","::anything::") will return the previous and next pages from the home page, and
#' c("::anything::","::anything::","Home") will return the two previous pages leading to the home page.
#' @param top Number of rows to return (defaults to 1000)
#' @param start Start row if you do not want to start at #1
#' @param segment.id Id(s) of Adobe Analytics segment to retrieve the report for
#' @param expedite Set to TRUE to expedite the processing of this report
#' @param interval.seconds How long to wait between attempts
#' @param max.attempts Number of API attempts before stopping
#' @param validate whether to submit report definition for validation before requesting the data.
#' @param enqueueOnly only enqueue the report, don't get the data. returns report id, which you can later use to get the data
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return Data frame
#'
#' @examples
#' \dontrun{
#' pathpattern <- c("Home Page","::anything::","::anything::","::anything::")
#' queue_pathing_pages <- QueuePathing("your_report_suite",
#'                                     "2014-04-01",
#'                                     "2014-04-20",
#'                                     metric="pageviews",
#'                                     element="page",
#'                                     pathpattern
#'                                     )
#' enqueued.report.id <- QueuePathing("your_report_suite",
#'                                     "2014-04-01",
#'                                     "2014-04-20",
#'                                     metric="pageviews",
#'                                     element="page",
#'                                     pathpattern,
#'                                     enqueueOnly=TRUE
#'                                     )
#' }
#'
#' @export

QueuePathing <- function(reportsuite.id, date.from, date.to, metric, element, pattern,
                        top=1000, start=1,
                        segment.id='', expedite=FALSE,interval.seconds=5,max.attempts=120,validate=TRUE,enqueueOnly=FALSE) {

  # build JSON description
  # we have to use unbox to force jsonlist not put strings into single-element arrays
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- unbox(date.from)
  report.description$reportDescription$dateTo <- unbox(date.to)
  report.description$reportDescription$reportSuiteID <- unbox(reportsuite.id)

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  report.description$reportDescription$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  report.description$reportDescription$elementDataEncoding <- unbox("utf8")

  #If segment is null, apply the standard segment unbox function
    if(as.list(segment.id)[1]==''){
    report.description$reportDescription$segment_id <- unbox(segment.id)
      }
  #If segment is not null, treat it like a list of metrics.
    else{
    report.description$reportDescription$segments <- data.frame( id = segment.id)

    }
  if(expedite!=FALSE) {
    report.description$reportDescription$expedite <- unbox(expedite)
  }
  report.description$reportDescription$metrics = data.frame(id = metric)
  report.description$reportDescription$elements = list(list(id = unbox(element),
                                                            top = unbox(top),
                                                            startingWith = unbox(start),
                                                            pattern = as.list(pattern)))

  report.data <- SubmitJsonQueueReport(toJSON(report.description),interval.seconds=interval.seconds,max.attempts=max.attempts,validate=validate,enqueueOnly=enqueueOnly)

  return(report.data)

}
