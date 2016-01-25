#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get video settings for the specified report suites.
#'
#' @title Get Video Settings for a Report Suite(s)
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
#' vidsettings<- GetVideoSettings("your_report_suite")
#'
#' vidsettings2 <- GetVideoSettings(report_suites$rsid)
#' }

GetVideoSettings<- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetVideoSettings")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$video_settings[[1]]) == 0) {
      return(print("No Video Settings Defined For This Report Suite"))
    }

  video_settings_df <- response$video_settings
  response$video_settings <- NULL

  accumulator <- cbind(response, video_settings_df)


  return(accumulator)

}