#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Gets success event definitions for the specified report suite(s).
#' Useful to audit or document a report suite or company in Adobe Analytics.
#'
#' @title Get Success Events Associated with a Report Suite
#'
#' @param reportsuite.ids report suite id (or list of report suite ids)
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
#' events <- GetSuccessEvents("your_report_suite")
#'
#' events2 <- GetSuccessEvents(report_suites$rsid)
#' }

GetSuccessEvents <- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  valid.successevents <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetEvents")

  successevents.formatted <- data.frame()
  for (i in 1:length(valid.successevents$rsid) ) {
    valid.successevents$events[[i]]$report_suite <- valid.successevents$rsid[[i]]
    valid.successevents$events[[i]]$site_title <- valid.successevents$site_title[[i]]
    valid.successevents$events[[i]]$ecommerce_level <- valid.successevents$ecommerce_level[[i]]
    if(nrow(successevents.formatted)==0) {
      successevents.formatted <- valid.successevents$events[[i]]
    } else {
      successevents.formatted <- rbind.fill(successevents.formatted,valid.successevents$events[[i]])
    }
  }

  return(successevents.formatted)

}