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
  request.body <- toJSON(list(rsid=unbox(reportsuite.ids)))
  results <- ApiRequest(body=request.body,func.name="ReportSuite.GetRealTimeSettings")
  
  #Test if there is a non-zero set of results
  if(length(results$correlations) == 0){
    return(print("No Real-Time Configuration Previously Set"))
  }
  
  #Remove the data frame from the list
  results_df <- results[[1]]
  
  #Fix ending column from character vector
  fixed_end_column <- ldply(results_df$elements)
  names(fixed_end_column) <- c("primary.dimension", "secondary.dimension", "tertiary.dimension")
  
  #Drop elements column
  results_df$elements <- NULL
  
  #Bind two data frames together and return
  return(cbind(results_df, fixed_end_column))
  
} #Ending bracket for function