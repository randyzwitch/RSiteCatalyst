#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get Virtual Report Suite defintions.
#'
#' @param reportsuite.ids report suite id (or list of report suite ids)
#'
#' @title Get Virtual Report Suite Settings
#'
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#' @return Data frame
#'
#' @examples
#' \dontrun{
#' virtualsettings <- GetVirtualReportSuiteSettings("your_report_suite")
#'
#' virtualsettings2 <- GetVirtualReportSuiteSettings(report_suites$rsid)
#'
#'}
#' @export

GetVirtualReportSuiteSettings <- function(reportsuite.ids) {

  report.description <- c()
  report.description$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  report.description$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  report.description$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(report.description),func.name="ReportSuite.GetVirtualReportSuiteSettings")

  return(response)

}