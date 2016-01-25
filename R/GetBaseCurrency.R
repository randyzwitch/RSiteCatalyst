#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get base currency for the specified report suites.
#'
#' @title Get Base Currency for a Report Suite(s)
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
#' currency <- GetBaseCurrency("your_report_suite")
#'
#' currency2 <- GetBaseCurrency(report_suites$rsid)
#' }

GetBaseCurrency <- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetBaseCurrency")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$base_currency[[1]]) == 0) {
      return(print("No Currency Defined For This Report Suite"))
    }

  return(response)

}