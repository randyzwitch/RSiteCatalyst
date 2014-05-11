#' Save Configuration for Real-Time Report
#' 
#' Selects the metrics and elements (dimensions) on which you want real time
#' reports enabled. Realtime configuration changes take 15 minutes to be
#' reflected in reports.
#' 
#' 
#' SaveRealTimeSettings should be called each time you want to modify the
#' structure of your real-time reports. If you are unsure of your current setup
#' of your real-time reports, use GetRealTimeSettings to find out your
#' current setup.
#' 
#' Changes can take up to 15 minutes to be reflected.
#' 
#' @param reportsuite.ids Report Suite ID
#' 
#' @return Message returned to console
#' @seealso \code{\link{GetRealTimeSettings}} \cr
#' @keywords SaveRealTimeSettings
#' @examples
#' 
#' \dontrun{
#' 
#'    }
#' 
#' @export
#' 
SaveRealTimeSettings <- function (reportsuite.ids="") {
  
  #API request
  #request.body <- toJSON(list(rsid=unbox(reportsuite.ids)))
  
  #Make error message/check against having 3 elements when ui_report = TRUE
  
  request.body <-
    '{
        "real_time_settings":[
          {"metric":"instances",
           "elements":["prop2", "searchenginekeyword", "geocountry"],
           "min_granularity":"1",
           "name":"test6",
           "ui_report":"1"
          },
          {"metric":"instances",
           "elements":["prop2", "referringdomain", "geocountry"],
           "min_granularity":"1",
           "name":"test2",
           "ui_report":"1"
          },
          {"metric":"instances",
           "elements":["prop3", "referringdomain", "geocountry"],
           "min_granularity":"1",
           "name":"test3",
           "ui_report":"1"
          }],
          "rsid_list":["zwitchdev"]
          }'

  #skip.queue parameter returns raw response
  results <- ApiRequest(body=request.body,func.name="ReportSuite.SaveRealTimeSettings", skip.queue=TRUE)
  
  #If a valid response returned, then print success; else, print code/message
  if(results$status_code == 200){
    print("Configuration Saved. Per API documentation, it can take up to 15mins for report to become active")
  } else{
    warning(sprintf("Status code %s: %s", results$status_code, results$statusmessage))
  }

} #End function bracket