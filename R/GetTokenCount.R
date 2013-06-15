#GetTokenCount - Find out how many tokens have been used this month
#and how many total are available each month

GetTokenCount <- function() {

json <-postRequest("Company.GetTokenCount")

#Check status of response, if good parse and convert to list
#This should probably be factored out into a JSON checking function

if(json$status == 200) {
  
  #Result returned answer
  result <- unlist(content(json))
  
  pct <- result/10000 #pct remaining
  
  #Print statement of tokens used and remaining
  print(paste("Your company has ", result, " tokens remaining this month (", sprintf("%.1f%%", pct*100), " remaining)", sep=""))
  
} else {
  #Error checking function
  stop(jsonResponseError(json$status))
}


} #End function bracket