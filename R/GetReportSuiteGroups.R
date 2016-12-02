#' @details Requires a single report suite id
#'
#' @description Retrieves a list of permission groups assigned to the specified report suite
#'
#' @title Get Report Suite Groups for a specific report suite
#'
#' @param reportsuite.id Report Suite ID
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
#' rsg <- GetReportSuiteGroups("your-report-suite")
#'
#'
#' }

GetReportSuiteGroups <- function(reportsuite.id) {

  request.body <- c()
  request.body$rsid  <- unbox(reportsuite.id)

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="Permissions.GetReportSuiteGroups")
  r_ <- cbind(response$rsid, response$site_title, response$all_report_suite_access_group_list)
  r_ <- rename(r_, replace = c("response$rsid" = "rsid", "response$site_title" = "site_title"))
  return(r_)

}