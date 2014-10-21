#' @details This function requires having a character string with a valid Report
#' Suite specified. You can specify any report suite you want, as all report 
#' suites have same tracking server.
#'
#' @description Get tracking server associated with a namespace (company).
#'
#' @param reportsuite.id report suite id
#' 
#' @title Get Tracking Server Associated with a Namespace (Company)
#' 
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#' @return Data frame
#' 
#' @examples
#' \dontrun{
#' ts <- GetTrackingServer("your_report_suite")
#' 
#' 
#'}
#' @export

GetTrackingServer <- function(reportsuite.id) {
  
  report.description <- c()
  report.description$rsid <- unbox(reportsuite.id)

  servers <- ApiRequest(body=toJSON(report.description),func.name="Company.GetTrackingServer")

  return(as.data.frame(servers))
}