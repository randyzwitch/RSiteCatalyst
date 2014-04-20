#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get Commerce Variables (eVars) Associated with a Report Suite.
#'
#' @param reportsuite.ids report suite id (or list of report suite ids)
#' 
#' @title Get Commerce Variables (eVars) Associated with a Report Suite
#' 
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#' @return Data frame
#' 
#' @examples
#' \dontrun{
#' evars <- GetEvars("your_report_suite")
#' 
#' evars2 <- GetEvars(report_suites$rsid)
#'
#'}
#' @export

GetEvars <- function(reportsuite.ids) {
  
  report.description <- c()
  report.description$rsid_list <- reportsuite.ids

  valid.evars <- ApiRequest(body=toJSON(report.description),func.name="ReportSuite.GetEvars")

  evars.formatted <- data.frame()
  for (i in 1:length(valid.evars$rsid) ) {
    valid.evars$evars[[i]]$report_suite <- valid.evars$rsid[[i]]
    if(nrow(evars.formatted)==0) {
      evars.formatted <- valid.evars$evars[[i]]
    } else {
      evars.formatted <- rbind.fill(evars.formatted,valid.evars$evars[[i]])
    }
  }

  return(evars.formatted)

}