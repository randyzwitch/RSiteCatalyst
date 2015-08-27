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
#' @param segment.id Id of Adobe Analytics segment to retrieve the report for
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
#' pathpattern <- c("Home Page","::anything::","::anything::","::anything::")
#' queue_pathing_pages <- QueuePathing("your_report_suite", 
#'                                     "2014-04-01", 
#'                                     "2014-04-20", 
#'                                     metric="pageviews", 
#'                                     element="page", 
#'                                     pathpattern
#'                                     )
#' 
#' }
#'
#' @export

QueuePathing <- function(reportsuite.id, date.from, date.to, metric, element, pattern,
                        top=1000, start=1,
                        segment.id='', expedite=FALSE,interval.seconds=5,max.attempts=120,validate=TRUE) {
  
  # build JSON description
  # we have to use unbox to force jsonlist not put strings into single-element arrays
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- unbox(date.from)
  report.description$reportDescription$dateTo <- unbox(date.to)
  report.description$reportDescription$reportSuiteID <- unbox(reportsuite.id)
  if(segment.id!="") { 
    report.description$reportDescription$segment_id <- unbox(segment.id) 
  }
  if(expedite!=FALSE) { 
    report.description$reportDescription$expedite <- unbox(expedite) 
  }
  report.description$reportDescription$metrics = data.frame(id = metric)
  report.description$reportDescription$elements = list(list(id = unbox(element), 
                                                            top = unbox(top), 
                                                            startingWith = unbox(start), 
                                                            pattern = as.list(pattern)))

  report.data <- SubmitJsonQueueReport(toJSON(report.description),interval.seconds=interval.seconds,max.attempts=max.attempts,validate=validate)

  return(report.data) 

}  
