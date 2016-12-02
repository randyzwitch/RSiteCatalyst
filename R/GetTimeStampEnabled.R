#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get whether Time Stamp functionality enabled for the specified report suites.
#'
#' @title Get Time Stamp Enabled for a Report Suite(s)
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
#' tse <- GetTimeStampEnabled("your_report_suite")
#'
#' tse2 <- GetTimeStampEnabled(report_suites$rsid)
#' }

GetTimeStampEnabled <- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetTimeStampEnabled")

  return(response)

}