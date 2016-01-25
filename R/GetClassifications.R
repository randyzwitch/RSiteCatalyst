#' @details Retrieves a list of classifications (associated with the specified element) for each of the specified report suites.
#' Function attempts to flatten classifications as best as possible; may return data frame having a nested list as a column if classification
#' is sufficiently complex.
#'
#'
#' @description Retrieves a list of classifications (associated with the specified element) for each of the specified report suites.
#'
#' @title Get Classifications for Selected Report Suite Elements
#'
#' @param reportsuite.ids Single report suite id or list of report suites
#' @param elements Optional. List of existing elements you want to use in combination with an additional metric
#'
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' classifications <- GetClassifications(c("prod", "dev"), "trackingcode")
#'
#' }


GetClassifications <- function(reportsuite.ids, elements=c()) {

if(length(elements) > 0){
  rd <- toJSON(list(rsid_list=reportsuite.ids, element_list=elements, locale=unbox(AdobeAnalytics$SC.Credentials$locale), elementDataEncoding=unbox("utf8")))
} else {
  rd <- toJSON(list(rsid_list=reportsuite.ids, locale=unbox(AdobeAnalytics$SC.Credentials$locale), elementDataEncoding=unbox("utf8")))
}

#Make API call
raw.response <- ApiRequest(body=rd,func.name="ReportSuite.GetClassifications")

#Parse first level of classification
accumulator <- data.frame()
classifications_list <- raw.response$element_classifications
raw.response$element_classifications <- NULL

for(i in 1:nrow(raw.response)){
  #Split get element classifications out of report
  temp <- cbind(raw.response[i,], classifications_list[i][[1]], row.names = NULL)
  accumulator <- rbind.fill(accumulator, temp)
}

accumulator <- rename(accumulator, c("name" = "element_name"))

#Parse second level of classification
accumulator2 <- data.frame()
classifications_list2 <- accumulator$classifications
accumulator$classifications <- NULL

for(i in 1:nrow(accumulator)){
  #Split get element classifications out of report
  temp <- cbind(accumulator[i,], classifications_list2[i][[1]], row.names = NULL)
  accumulator2 <- rbind.fill(accumulator2, temp)
}
accumulator2 <- rename(accumulator2, c("name" = "classification_name"))

return(accumulator2)

} #End function call


