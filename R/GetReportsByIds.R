#' @name GetReportsByIds
#'
#' @title Get EnQueued Reports by report ID
#'
#' @details This is a function for advanced users, after you've enqueued multiple reports
#' and want to get all of them when they're ready.
#'
#' @description Get reports for report ids provided as a list. These reports are previously enqueued.
#'
#' @param report.ids list of report ids that you've enqueued and want to retrieve the data for
#' @param interval.seconds How long to wait between attempts
#' @param max.attempts Number of API attempts before stopping
#' @param print.attempts Print each attempt to check if report is ready
#'
#' @return list of (report id and Data frame pairs)
#'
#' @examples
#' \dontrun{
#'
#' reports <- GetReportsByIds(list(12345678,87654321),print.attempts=FALSE)
#'
#' }
#'
#' @export
#'

GetReportsByIds <- function(report.ids,interval.seconds=10,max.attempts=300,print.attempts=TRUE) {
  reports <- list()
  attempts <- 0
  while (length(report.ids)>0 && attempts <= max.attempts) {
    queue <- GetQueue()
    # queue may be empty, so check:
    if (length(queue)!=0) queue = as.numeric(queue$reportID)
    completed <- setdiff(report.ids, queue)
    print(paste('Reports still processing:',length(report.ids)-length(completed)))
    for (report.id in completed) {
      print(paste('Fetching report id', report.id))

      report <- GetReport(report.id,print.attempts=print.attempts)
      reports[[length(reports)+1]] <- list('report.id'=report.id,'data.frame'=report)
      report.ids <- report.ids[report.ids!=report.id]
    }
    if (length(report.ids>0)) {
      Sys.sleep(interval.seconds)
    }
    attempts <- attempts + 1
  }
  return(reports)
}
