#getReportQueue - See what reports are in the queue
#Not sure if this should be public or private function




#' Get Number/ID of Reports in Queue
#' 
#' Requests the number of reports in the Report Queue, as well as the Report
#' ID.
#' 
#' Returns either a message to the console that no reports are in the Queue or
#' a list with the Report ID's.
#' 
#' @return Character string or List
#' @seealso \code{\link{CancelReport}}
#' @keywords report queue
#' @examples
#' 
#' \dontrun{  
#'     GetReportQueue()
#'     
#'     #Example Result
#'     "There are no reports in the Report Queue"
#'     }
#'     
#' 
GetReportQueue <- function() {

json <-postRequest("Report.GetReportQueue")

if(json$status == 200) {
result <- content(json)
} else {
  stop(jsonResponseError(json$status))
}

if(length(result) ==0) {
  return("There are no reports in the Report Queue")
} else {
return(result)
}

} #End function bracket
