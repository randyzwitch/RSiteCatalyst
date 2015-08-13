#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get privacy settings for the specified report suites. 
#' 
#' @title Get Privacy Settings for a Report Suite(s)
#' 
#' @param reportsuite.ids Report suite id (or list of report suite ids)
#'
#' @importFrom jsonlite toJSON
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' privacy <- GetPrivacySettings("your_report_suite")
#' 
#' privacy2 <- GetPrivacySettings(c("your_dev_suite", "your_prod_suite"))
#' 
#' }

GetPrivacySettings <- function(reportsuite.ids) {
  
  request.body <- c()
  request.body$rsid_list <- reportsuite.ids
  
  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetPrivacySettings")
  
  response <- cbind(response, ldply(response$privacy_settings, quickdf))
  response$privacy_settings <- NULL
  
  return(response)
  
}



