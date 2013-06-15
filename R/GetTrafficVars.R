#GetTrafficVars- Get props for a single or multiple report suites
#This one could use minor cleanup


GetTrafficVars <- function (report_suites) {

  
#Converts report_suites to JSON
if(length(report_suites)>1){
  report_suites <- toJSON(report_suites)
} else {
  report_suites <- toJSON(list(report_suites))
}

#API request
json <- postRequest("ReportSuite.GetTrafficVars",paste('{"rsid_list":', report_suites , '}'))

if(json$status == 200) {
#Convert JSON to list
results <- content(json)
} else {
  stop(jsonResponseError(json$status))
}

temp <- data.frame()
#Loop over report suite level 
for(report_suite in 1:length(results)){
    
  rsid_name <- results[[report_suite]]["rsid"]
  
  #Create dataframe for each suite based on report suite name with suffix _props
  temp <- rbind.fill(temp, cbind(rsid=rsid_name, ldply(results[[report_suite]]$traffic_vars, quickdf)))
  
  } #Ending bracket for report suite loop

return(temp)
} #Ending bracket for function

  

  











