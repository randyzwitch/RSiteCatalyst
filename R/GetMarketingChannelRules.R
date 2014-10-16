#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get marketing channel rules for the specified report suites. 
#' 
#' @title Get Marketing Channel Rules for a Report Suite(s)
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
#' expire <- GetMarketingChannelRules("your_report_suite")
#' 
#' expire2 <- GetMarketingChannelRules(report_suites$rsid)
#' }

GetMarketingChannelRules <- function(reportsuite.ids) {
  
  request.body <- c()
  request.body$rsid_list <- reportsuite.ids
  
  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetMarketingChannelRules")
  
  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$marketing_channel_rules[[1]]) == 0) {
      return(print("No Rules Defined For This Report Suite"))
    }
  
  
  
  return(response)


}