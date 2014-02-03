#Set real-time configration
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
