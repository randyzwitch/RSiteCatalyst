#' GetElements
#'
#' Gets valid elements for a report suite for the current user. 
#' This list is restricted by optionally specified existing elements, existing metrics and date granularity.
#'
#' @param reportsuite.ids single report suite id, or character vector of report suite ids
#' @param metrics list of existing metrics you want to use in combination with an additional metric
#' @param elements list of existing elements you want to use in combination with an additional metric
#' @param date.granularity granularity that you want to combine with an additional metric
#'
#' @importFrom jsonlite toJSON
#'
#' @return List of valid elements
#'
#' @export
#'
#' @examples
#' \dontrun{
#' elements.valid <- GetElements("your_report_suite",metrics=c('visitors','pageviews'),
#' elements=c('page','geoCountry'),date.granularity='day')
#' 
#' elements <- GetElements(c("your_prod_report_suite","your_dev_report_suite"))
#' }

GetElements <- function(reportsuite.ids, metrics=c(), elements=c(), date.granularity='') {
  
  valid.elements <- data.frame()
  for(reportsuite.id in reportsuite.ids) {
    request.body <- c()
    request.body$reportSuiteID <- jsonlite:::as.scalar(reportsuite.id)

    if(length(metrics)>0) { 
      request.body$reportDescription$existingElements <- metrics
    }
    if(length(elements)>0) { 
      request.body$reportDescription$existingElements <- elements
    }
    if(nchar(date.granularity)>0) { 
      request.body$reportDescription$dateGranularity <- jsonlite:::as.scalar(date.granularity) 
    }
    working.elements <- ApiRequest(body=toJSON(request.body),func.name="Report.GetElements")
    working.elements$rsid <- reportsuite.id
    if(length(valid.elements)){
      valid.elements <- rbind(valid.elements,working.elements)
    } else {
      valid.elements <- working.elements
    }
  }

  return(valid.elements)

}