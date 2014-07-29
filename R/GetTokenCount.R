#GetTokenCount - Find out how many tokens have been used this month
#and how many total are available each month



#' Get API Token Count
#' 
#' Requests number of API tokens remaining for the month.
#' 
#' NOTE: There is no longer a token limit on the Adobe Analytics API. This
#' function remains unchanged for historical purposes.
#' 
#' Returns the number of tokens remaining per month and provides percentage
#' remaining.
#' 
#' @return Character string
#' @seealso \code{\link{SCAuth}}
#' @keywords tokens
#' @examples
#' 
#' \dontrun{
#' 
#'     GetTokenCount()
#'     
#'     #Example Result
#'     "Your company has 9965 tokens remaining this month (99.7% remaining)"
#'  }   
#'     
#' 
GetTokenCount <- function() {

json <-postRequest("Company.GetTokenCount")

#Check status of response, if good parse and convert to list
#This should probably be factored out into a JSON checking function

if(json$status == 200) {
  
  #Result returned answer
  result <- fromJSON(content(json, "text"))
  
  pct <- result/10000 #pct remaining
  
  #Print statement of tokens used and remaining
  print(paste("Your company has ", result, " tokens remaining this month (", sprintf("%.1f%%", pct*100), " remaining)", sep=""))
  
} else {
  #Error checking function
  stop(jsonResponseError(json$status))
}


} #End function bracket
