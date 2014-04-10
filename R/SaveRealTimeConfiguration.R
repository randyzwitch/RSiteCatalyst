#Set real-time configration


#' Save Configuration for Real-Time Report
#' 
#' Selects the metrics and elements (dimensions) on which you want real time
#' reports enabled. Realtime configuration changes take 15 minutes to be
#' reflected in reports.
#' 
#' 
#' SaveRealTimeConfiguration should be called each time you want to modify the
#' structure of your real-time reports. If you are unsure of your current setup
#' of your real-time reports, use GetRealTimeConfiguration to find out your
#' current setup.
#' 
#' Changes can take up to 15 minutes to be reflected.
#' 
#' @param report_suite Report Suite ID
#' @param metric1 Metric to use in real-time report slot 1
#' @param elements1 Up to three elements to use for real-time report slot 1
#' @param metric2 Metric to use in real-time report slot 2
#' @param elements2 Up to three elements to use for real-time report slot 2
#' @param metric3 Metric to use in real-time report slot 3
#' @param elements3 Up to three elements to use for real-time report slot 3
#' @return Message returned to console
#' @seealso \code{\link{GetRealTimeConfiguration}} \cr
#' @keywords SaveRealTimeConfiguration
#' @examples
#' 
#' \dontrun{
#' 
#' SaveRealTimeConfiguration("keystonerandy", 
#' metric1 = "instances", 
#' elements1 = c("page", "referringdomain", "sitesection"),
#' metric2 = "revenue",
#' elements2 = c("referringdomain", "sitesection")
#' )
#'   
#' 
#'    }
#' 
SaveRealTimeConfiguration<- function (report_suite="", metric1="", elements1="", metric2="", elements2="", metric3="", elements3="") {

  #Error checking to make sure at least 1 set of reports is defined
  if(report_suite == ""){
    stop(print("report_suite is a required argument"))
  }
  
  if(metric1 == ""){
    stop(print("metric1 is a required argument"))
  }
  
  if(is.character(elements1) == FALSE){
    stop(print("elements1 is a required argument as a list"))
  }
  
#Build JSON string - single case
if(metric2 == "" && elements2 == "" && metric3 == "" && elements3 == "") {
elements1 = toJSON(elements1)
body <- sprintf('  
{
  "rsid": "%s",
  "correlations":[
    {
      "min_granularity":1,
      "metric":"%s",
      "elements":%s
    }
]}', report_suite, metric1, elements1) 
} else if (metric3 == "" && elements3 == "" ){

elements1 = toJSON(elements1)
elements2 = toJSON(elements2)
  
  body <- sprintf('  
{
  "rsid": "%s",
  "correlations":[
    {
      "min_granularity":1,
      "metric":"%s",
      "elements":%s
    },
    {
      "min_granularity":1,
      "metric":"%s",
      "elements":%s
    }
]}', report_suite, metric1, elements1, metric2, elements2)  
  
} else {
  
  elements1 = toJSON(elements1)
  elements2 = toJSON(elements2)
  elements3 = toJSON(elements3)
  
  body <- sprintf('  
{
  "rsid": "%s",
  "correlations":[
    {
      "min_granularity":1,
      "metric":"%s",
      "elements":%s
    },
    {
      "min_granularity":1,
      "metric":"%s",
      "elements":%s
    },
    {
      "min_granularity":1,
      "metric":"%s",
      "elements":%s
    }
]}', report_suite, metric1, elements1, metric2, elements2, metric3, elements3)
  
}

#Make API request
json <- postRequest("ReportSuite.SaveRealTimeConfiguration", body)

if(json$status == 400) {
  stop(print(content(json)$errors))
} else if(json$status == 200) {
  print("Configuration Saved. Per API documentation, it can take up to 15mins for report to become active")
}

} #End function bracket
