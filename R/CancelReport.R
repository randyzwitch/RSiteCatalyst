#cancelReport - function to cancel a report taking too long
#Currently, not called by any other function




#' Cancel a Report in the Report Queue
#' 
#' Cancels a report in the Report Queue
#' 
#' Returns either a console message that no reports are queued or a list of
#' reportID numbers
#' 
#' @param reportID Report ID obtained from GetReportQueue
#' @return Character string or List
#' @seealso \code{\link{GetReportQueue}}
#' @keywords cancel report
#' @examples
#' 
#'   
#' \dontrun{  
#'   
#'     CancelReport(12345678)
#'     
#'     #Example Result
#'     "Report 12345678 has been cancelled"
#'     
#'           }
#' 
#' @export CancelReport
CancelReport <- function(reportID) {
cancelled<- content(postRequest("Report.CancelReport",paste('{"reportID":', toJSON(reportID) , '}')))

if(cancelled[1] == 1){
  print(paste("Report", reportID, "has been cancelled", sep= " "))
} else {
  warning("Report may not be cancelled, this function not thoroughly tested")
}
}
