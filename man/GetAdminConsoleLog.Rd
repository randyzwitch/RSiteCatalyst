\name{GetAdminConsoleLog}
\alias{GetAdminConsoleLog}
\title{Admin Actions to Report Suite Settings}
\description{
    Get all Report Suite actions made by Admins during a given time period.
}
\usage{
    GetAdminConsoleLog(start_date, end_date, report_suites)
}
\arguments{
    \item{start_date}{
        Optional: Start date of report, in "YYYY-MM-DD" format. If argument not set in function call, defaults to Sys.Date() - 1
    }
    \item{end_date}{
        Optional: End date of report, in "YYYY-MM-DD" format. If argument not set in function call, defaults to Sys.Date()
    }
    \item{report_suites}{
        One or more Report Suites
    }
   
}
\details{
    This report is administrative in nature, for users interested in understanding how changes have been made to the SiteCatalyst implementation.
    
    Note that this API is limited to returning 1000 results per call. This is important to remember when passing a list of report suites or long time periods.
}
\value{
    Data Frame
}

\examples{
\dontrun{

admin_console <- GetAdminConsoleLog("2013-01-01", "2013-02-20",c("keystonerandy", 
"keystonejowanza", "keystonejoel"))

#Be careful of 1000 limit when passing entire list of rsid's
admin_console <- GetAdminConsoleLog("2012-01-01", "2013-02-20",report_suites$rsid)

   } 
    
}
\keyword{admin}