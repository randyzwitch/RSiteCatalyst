#GetAvailableElements- Get all elements for a single or multiple report suites
#This one could use minor cleanup, get DW elements hardcoded as "0" (no)



#' Get Available Elements within a Report Suite
#' 
#' Get All Available Elements for a given Report Suite. This list can be used
#' to inform other functions having an "element" parameter.
#' 
#' This function requires having a character vector with one or more valid
#' Report Suites specified.
#' 
#' @param report_suites Character vector containing one or more valid Report
#' Suite names
#' @return Data Frame
#' @references Official Adobe Documentation about Elements list\cr
#' https://developer.omniture.com/en_US/documentation/sitecatalyst-reporting/r-elements-1
#' @keywords elements
#' @examples
#' 
#' \dontrun{   
#' 
#'     GetAvailableElements("keystonejowanza")
#'     GetAvailableElements(c("keystonejowanza", "keystonerandy", "keystonetraining"))
#'     }
#'     
#'     
#' 
GetAvailableElements <- function (report_suites) {

  
#Converts report_suites to JSON
if(length(report_suites)>1){
  report_suites <- toJSON(report_suites)
} else {
  report_suites <- toJSON(list(report_suites))
}

#API request
json <- postRequest("ReportSuite.GetAvailableElements",paste('{"return_datawarehouse_elements":"0","rsid_list":', report_suites , '}'))

if(json$status == 200){
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
  temp <- rbind.fill(temp, cbind(rsid=rsid_name, ldply(results[[report_suite]]$available_elements, quickdf)))
  
  } #Ending bracket for report suite loop
return(temp)
} #Ending bracket for function

  

  











