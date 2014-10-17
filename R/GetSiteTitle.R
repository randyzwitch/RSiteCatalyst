#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get site title for the specified report suites. 
#' 
#' @title Get Site Title for a Report Suite(s)
#' 
#' @param reportsuite.ids Report suite id (or list of report suite ids)
#'
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sitetitle <- GetSiteTitle("your_report_suite")
#' 
#' sitetitle2 <- GetSiteTitle(report_suites$rsid)
#' }

GetSiteTitle <- function(reportsuite.ids) {
  
  request.body <- c()
  request.body$rsid_list <- reportsuite.ids
  
  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetSiteTitle")
  
  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$site_title[[1]]) == 0) {
      return(print("No Site Title Defined For This Report Suite"))
    }

  return(response)

}