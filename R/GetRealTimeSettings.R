#' Get Current Settings for Real-Time Reports
#' 
#' Get Current Settings for Real-Time Reports
#' 
#' 
#' GetRealTimeSettings returns a Data Frame with the current set up of
#' real-time reports within the Adobe Analytics Real-Time API. 
#' 
#' To change configuration settings, use SaveRealTimeConfiguration function.
#' 
#' @param reportsuite.ids Report Suite ID
#' @return Data Frame
#' @seealso \code{\link{SaveRealTimeSettings}} \cr
#' @keywords GetRealTimeSettings
#' @examples
#' 
#' \dontrun{
#'   
#'   GetRealTimeSettings("your_report_suite")
#' 
#'    }
#' 
#' @export
#' 
GetRealTimeSettings<- function (reportsuite.ids) {
  
  #API request
  request.body <- toJSON(list(rsid_list=reportsuite.ids))
  results <- ApiRequest(body=request.body,func.name="ReportSuite.GetRealTimeSettings")
  
  df <- data.frame()
  for(i in 1:nrow(results)){
    temp <- results[i,]
    if(nrow(temp$real_time_settings[[1]]) == 0){
      
      temp2 <- cbind(data.frame(rsid = temp$rsid), 
                     data.frame(site_title = temp$site_title)
      )
      temp3 <- temp2
      
    } else {
      
      temp2 <- cbind(data.frame(rsid = temp$rsid), 
                     data.frame(site_title = temp$site_title), 
                     temp$real_time_settings)
      
      temp3 <- cbind(temp2, ldply(temp2$elements))
      temp3$elements <- NULL
      
      
    }
    df <- rbind.fill(df, temp3)
  }
  
  df <- rename(df, c("V1" = "primary.dimension", 
               "V2" = "secondary.dimension", 
               "V3" = "tertiary.dimension"),
         warn_missing = FALSE)
  return(df)
  
} #Ending bracket for function