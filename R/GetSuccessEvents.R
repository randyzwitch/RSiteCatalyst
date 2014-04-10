#GetSuccessEvents- Get events for a single or multiple report suites
#Some of this code could be better




#' Get Success Events Associated with a Report Suite
#' 
#' Get Success Events associated with one or more Report Suites.
#' 
#' This function requires having a character vector with one or more valid
#' Report Suites specified.
#' 
#' @param report_suites Character vector containing one or more valid Report
#' Suite names
#' @return Data Frame
#' @keywords events
#' @examples
#' 
#' \dontrun{    
#'     
#'     GetSuccessEvents("keystonejowanza")
#'     GetSuccessEvents(c("keystonejowanza", "keystonerandy", "keystonetraining"))
#'     }
#'     
#'     
#' 
#' @export GetSuccessEvents
GetSuccessEvents <- function (report_suites) {

  
#A bit of slop to format report_suites into JSON
if(length(report_suites)>1){
  report_suites <- toJSON(report_suites)
} else {
  report_suites <- toJSON(list(report_suites))
}

#API request
json <- postRequest("ReportSuite.GetSuccessEvents",paste('{"rsid_list":', report_suites , '}'))

if(json$status == 200) {
#Convert JSON result to list
results <- content(json)
} else {
  stop(jsonResponseError(json$status))
}

#Loop over report suite level to build df

temp <- data.frame()
for(report_suite in 1:length(results)){
  
  rsid_name <- results[[report_suite]][[1]]
  
  #Create dataframe using rbind.fill in case columns aren't same in all df
  temp <- rbind.fill(temp, cbind(rsid=rsid_name, ldply(results[[report_suite]]$events, quickdf)))
  
  } #Ending bracket for report suite loop

return(temp)
} #Ending bracket for function

  

  












