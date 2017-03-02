#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get previous server calls for the specified report suites.
#'
#' @title Get Previous Server Calls for a Report Suite(s)
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
#' gpsc <- GetPreviousServerCalls("your_report_suite")
#'
#' gpsc2 <- GetPreviousServerCalls(report_suites$rsid)
#' }

GetPreviousServerCalls <- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetPreviousServerCalls")
  
  parsed <- ldply(response, quickdf)
  names(parsed) <- c("rsid", "average_server_calls", "peak_day_server_calls_date", "peak_day_server_calls")
  return(parsed)

}