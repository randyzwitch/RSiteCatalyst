#cancelReport - function to cancel a report taking too long
#Currently, not called by any other function


CancelReport <- function(reportID) {
cancelled<- content(postRequest("Report.CancelReport",paste('{"reportID":', toJSON(reportID) , '}')))

if(cancelled[1] == 1){
  print(paste("Report", reportID, "has been cancelled", sep= " "))
} else {
  warning("Report may not be cancelled, this function not thoroughly tested")
}
}