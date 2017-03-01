#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get Click Map Settings for the specified report suites.
#'
#' @title Get Click Map Settings for a Report Suite(s)
#'
#' @param reportsuite.ids Report suite id (or list of report suite ids)
#'
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cmsettings<- GetClickMapReporting("your_report_suite")
#'
#' cmsettings2 <- GetClickMapReporting(report_suites$rsid)
#' }

GetClickMapReporting <- function(reportsuite.ids) {

    output <- data.frame()
    for(rs in reportsuite.ids){
      request.body <- c()
      request.body$rsid_list <- rs
  
      #Hack in locale, every method calls ApiRequest so this hopefully works
      #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
      request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
      request.body$elementDataEncoding <- unbox("utf8")
  
      d <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetClickMapReporting")
      
      d_parsed <- data.frame(rsid = rs,
                             allow_configuration = d$allow_configuration,
                             all_clickmap_enabled = d$all_clickmap_enabled,
                             clickmap_v3_download_enabled = d$clickmap_v3_download_enabled,
                             selected_metric = d$selected_metric,
                             can_upgrade_30_to_31 = d$can_upgrade_30_to_31
      )
      #Create df from list for available metrics
      ametric <- ldply(d$available_metrics)
      names(ametric) <- c("id", "metric")
      
      #cbind/recycling atomic elements
      d_parsed_ <- cbind(d_parsed, ametric)
      
      output <- rbind.fill(output, d_parsed_)
    }
    
    return(output)

}