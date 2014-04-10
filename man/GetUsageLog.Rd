\name{GetUsageLog}
\alias{GetUsageLog}
\title{Admin Actions, Logins, and Reports Accessed}
\usage{
GetUsageLog(date_from = as.character(Sys.Date() - 1),
  date_to = as.character(Sys.Date()), localtime = FALSE)
}
\arguments{
  \item{date_from}{Optional: Start date of report, in
  "YYYY-MM-DD" format. If argument not set in function
  call, defaults to Sys.Date() - 1}

  \item{date_to}{Optional: End date of report, in
  "YYYY-MM-DD" format. If argument not set in function
  call, defaults to Sys.Date()}

  \item{localtime}{Optional. If TRUE, timestamp converted
  to local time. Otherwise, UNIX time in seconds.}
}
\value{
Data Frame
}
\description{
Creates data frame containing all Report Suite actions
(Admin, Logins, Reports Accessed) during a given time
period.
}
\details{
This report is administrative in nature, for users
interested in understanding how internal constituents are
using SiteCatalyst. This report is also a record of all
Admin changes to an account.
}
\examples{
\dontrun{

    #Timestamp in local time
    GetUsageLog("2013-01-01", "2013-02-13", TRUE)

    #UNIX time in seconds for timestamp
    GetUsageLog("2013-01-01", "2013-02-13")
 }
}
\keyword{admin}

