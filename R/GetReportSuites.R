#Get Report Suites for the account belonging with the user/secret combo

GetReportSuites<-function(){
  
  #Get Report Suites
  json <- postRequest("Company.GetReportSuites")

  
  
  if(json$status == 200){ 
    
  #Convert to list
  result <- content(json)
    
  #Data Frame from list
  temp <- as.data.frame(t(ldply(result, quickdf)))
  temp <- temp[2:nrow(temp),]
  names(temp) <- c("rsid", "site_title")

  return(temp)
  } else {
    
    return(jsonResponseError(json$status))
  }

}


