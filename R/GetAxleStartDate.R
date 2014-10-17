#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get cutover date from SC14 to SC15 for the specified report suites. 
#' 
#' @title Get Cutover Date from SC14 to SC15 for a Report Suite(s)
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
#' switch <- GetAxleStartDate("your_report_suite")
#' 
#' switch2 <- GetAxleStartDate(report_suites$rsid)
#' }

GetAxleStartDate <- function(reportsuite.ids) {
  
  request.body <- c()
  request.body$rsid_list <- reportsuite.ids
  
  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetAxleStartDate")
  
  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$axle_start_date[[1]]) == 0) {
      return(print("No Start Date Defined For This Report Suite"))
    }

  return(response)

}