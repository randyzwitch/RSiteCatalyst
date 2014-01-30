#' GetMetrics
#'
#' Gets valid metrics for current user, valid with optionally specified existing metrics, elements and date granularity
#'
#' @param reportsuite.id report suite id
#' @param metrics list of existing metrics you want to use in combination with an additional metric
#' @param elements list of existing elements you want to use in combination with an additional metric
#' @param date.granularity granularity that you want to combine with an additional metric
#'
#' @return List of valid metrics
#'
#' @export
#'
#' @examples
#' metrics.valid <- GetMetrics("your_report_suite",metrics=c('visitors','pageviews'),elements=c('page','geoCountry'),date.granularity='day')
#'

GetMetrics <- function(reportsuite.id, metrics=c(), elements=c(), date.granularity='') {
  
  request.body <- c()
  request.body$reportSuiteID <- jsonlite:::as.scalar(reportsuite.id)

  if(length(metrics)>0) { 
    request.body$reportDescription$existingMetrics <- metrics
  }
  if(length(elements)>0) { 
    request.body$reportDescription$existingElements <- elements
  }
  if(nchar(date.granularity)>0) { 
    request.body$reportDescription$dateGranularity <- jsonlite:::as.scalar(date.granularity) 
  }

  valid.metrics <- ApiRequest(body=toJSON(request.body),func.name="Report.GetMetrics")

  return(valid.metrics)

}