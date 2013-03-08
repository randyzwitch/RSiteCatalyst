#GetStatus - function to check whether Reports submitted are ready or still 
#waiting to return
#returns a string intended be used as part of IF statements


GetStatus <- function(reportID) {
checkStatus <- content(postRequest("Report.GetStatus",paste('{"reportID":', toJSON(reportID) , '}')))
return(checkStatus[[1]])
}