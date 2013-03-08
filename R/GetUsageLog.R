#GetUsageLog - By report suite, get all user actions for a period of time

  
GetUsageLog <- function(date_from=as.character(Sys.Date()-1), date_to=as.character(Sys.Date()), localtime=FALSE) {

#Make sure dates are in right order
if(date_from > date_to) {
  return(print("Error:  'date_from' is more recent than 'date_to'"))
}
  
#Make API call - Numerous parameters left out, this returns all info for all users, instead of single user
json <- postRequest("Logs.GetUsageLog", paste('{"date_from":', toJSON(date_from), "," , '"date_to":', toJSON(date_to), '}'))


#Validate that API answer is Success
if(json$status != 200) {
  return(jsonResponseError(json$status))
} else {
  #Convert JSON raw into list
  results <- content(json)
}

if(length(results) > 0) {
#Return formatted dataframe
  temp <- ldply(results, quickdf)
  
  #Convert to local time if flag set to TRUE
  if(localtime) {
  temp$timestamp <- as.POSIXct(as.numeric(temp$timestamp), origin="1970-01-01")
  }
  
return(temp)

} else {
  return(print("There are no results for the timeperiod selected"))
}


}

