#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get localization for the specified report suites. 
#' 
#' @title Get Localization for a Report Suite(s)
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
#' local <- GetLocalization("your_report_suite")
#' 
#' local2 <- GetLocalization(report_suites$rsid)
#' }

GetLocalization <- function(reportsuite.ids) {
  
  request.body <- c()
  request.body$rsid_list <- reportsuite.ids
  
  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetLocalization")
  
  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$localization[[1]]) == 0) {
      return(print("No Currency Defined For This Report Suite"))
    }

  return(response)

}