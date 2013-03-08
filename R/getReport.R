#GetReport - Get report after GetStatus returns "done"

getReport <- function(reportID) {
return(content(postRequest("Report.GetReport",paste('{"reportID":', toJSON(reportID) , '}'))))
}