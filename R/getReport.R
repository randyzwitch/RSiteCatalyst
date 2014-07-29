#GetReport - Get report after GetStatus returns "done"

getReport <- function(reportID) {

  prelim <- content(postRequest("Report.GetReport",paste('{"reportID":', toJSON(reportID) , '}')), "text")
  json_to_r <- rjson::fromJSON(prelim)
  
  return(json_to_r)
}