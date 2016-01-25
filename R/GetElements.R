#' @description Get valid elements for a report suite for the current user.
#' This list is restricted by optionally specified existing elements, existing metrics and date granularity.
#'
#' @details This function requires a character vector with one or more valid Report Suites specified.
#'
#' @title Get Valid Elements for a Report Suite
#' @param reportsuite.ids Single report suite id, or character vector of report suite ids
#' @param metrics List of existing metrics you want to use in combination with an additional metric
#' @param elements List of existing elements you want to use in combination with an additional metric
#' @param date.granularity Granularity that you want to combine with an additional metric
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' elements.valid <- GetElements("your_report_suite",
#'                               metrics=c('visitors','pageviews'),
#'                               elements=c('page','geoCountry'),
#'                               date.granularity='day')
#'
#' elements <- GetElements(c("your_prod_report_suite","your_dev_report_suite"))
#' }

GetElements <- function(reportsuite.ids, metrics=c(), elements=c(), date.granularity='') {

  valid.elements <- data.frame()
  for(reportsuite.id in reportsuite.ids) {
    request.body <- c()
    request.body$reportSuiteID <- unbox(reportsuite.id)

    #Hack in locale, every method calls ApiRequest so this hopefully works
    #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
    request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
    request.body$elementDataEncoding <- unbox("utf8")

    if(length(metrics)>0) {
      request.body$existingMetrics <- metrics
    }
    if(length(elements)>0) {
      request.body$existingElements <- elements
    }
    if(nchar(date.granularity)>0) {
      request.body$dateGranularity <- unbox(date.granularity)
    }
    working.elements <- ApiRequest(body=toJSON(request.body),func.name="Report.GetElements")
    working.elements$rsid <- reportsuite.id
    if(length(valid.elements)){
      valid.elements <- rbind.fill(valid.elements,working.elements)
    } else {
      valid.elements <- working.elements
    }
  }

  return(valid.elements)

}