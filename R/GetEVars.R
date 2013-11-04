#GetEVars- Get evars for a single or multiple report suites
#This one could use minor cleanup

GetEVars <- function (report_suites) {

#Converts report_suites to JSON
if(length(report_suites)>1){
  report_suites <- toJSON(report_suites)
} else {
  report_suites <- toJSON(list(report_suites))
}

#API request
json <- postRequest("ReportSuite.GetEVars",paste('{"rsid_list":', report_suites , '}'))

if(json$status == 200) {
#Convert JSON to list, clean through null values before list creation
results <- fromJSON(str_replace_all(content(json, as="text"), "null", 0))
} else {
  stop(jsonResponseError(json$status))
}

evar_df <- data.frame()
#Loop over report suite level 
for(report_suite in 1:length(results)){
  
  rsid_name <- results[[report_suite]]["rsid"]
  
  #Save each section of list into temp object
  temp = results[[report_suite]]
  
  #Traverse down EVar section for each temp object to collapse structs
  for(j in 1:length(temp$evars)){
    for(k in 1:length(temp$evars[[j]])){
      if(length(temp$evars[[j]][[k]]) > 1){
        temp$evars[[j]][[k]] <- paste(temp$evars[[j]][[k]], collapse = ",")
      } #End of if statement
    } #End of k loop
    
  } #End of j loop
  
  #Create dataframe for each suite based on report suite name with suffix _eVars
  evar_df <- rbind.fill(evar_df, cbind(rsid=rsid_name, ldply(temp$evars, quickdf)))
  
  
  } #Ending bracket for report suite loop

return(evar_df)
} #Ending bracket for function

  

  











