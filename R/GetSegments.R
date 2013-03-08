#GetSegments- Get segments for a single or multiple report suites
#This one could use minor cleanup


GetSegments <- function (report_suites) {

#Converts report_suites to JSON
if(length(report_suites)>1){
  report_suites <- toJSON(report_suites)
} else {
  report_suites <- toJSON(list(report_suites))
}

#API request
json <- postRequest("ReportSuite.GetSegments",paste('{"rsid_list":', report_suites , '}'))

if(json$status== 200) {
#Convert JSON to list, clean through null values before list creation
results <- fromJSON(str_replace_all(content(json, as="text"), "null", 0))
} else {
  return(jsonResponseError(json$status))
}

temp <- data.frame()
#Loop over report suite level 
for(report_suite in 1:length(results)){
  
  rsid_name <- results[[report_suite]]["rsid"]
  
  #If to check if there are segments in a report suite
  if(nrow(ldply(results[[report_suite]]$sc_segments, quickdf)) > 0) {
  #Create dataframe for each suite based on report suite name with suffix _eVars
  temp <- rbind.fill(temp, cbind(rsid=rsid_name, ldply(results[[report_suite]]$sc_segments, quickdf)))
  }
  
  } #Ending bracket for report suite loop

return(temp)
} #Ending bracket for function



  











