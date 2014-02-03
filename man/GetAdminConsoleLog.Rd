\name{GetAdminConsoleLog}
\alias{GetAdminConsoleLog}
\title{Admin Actions to Report Suite Settings}
\description{
    Get all Report Suite actions made by Admins during a given time period.
}
\usage{
    GetAdminConsoleLog(report_suites, start_date, end_date)
}
\arguments{
    \item{report_suites}{
        One or more Report Suites
    }
    \item{start_date}{
        Optional: Start date of report, in "YYYY-MM-DD" format. If argument not set in function call, defaults to Sys.Date() - 1
    }
    \item{end_date}{
        Optional: End date of report, in "YYYY-MM-DD" format. If argument not set in function call, defaults to Sys.Date()
    }

}
\details{
    DEPRECATED. This method removed by Adobe, with no replacement.
    
    This report is administrative in nature, for users interested in understanding how changes have been made to the SiteCatalyst implementation.
    
    Note that this API is limited to returning 1000 results per call. This is important to remember when passing a list of report suites or long time periods.
}
\value{
    Data Frame
}

\examples{
\dontrun{

admin_console <- GetAdminConsoleLog(c("keystonerandy", 
"keystonejowanza", "keystonejoel"), "2013-01-01", "2013-02-20")

#Be careful of 1000 limit when passing entire list of rsid's
admin_console <- GetAdminConsoleLog(report_suites$rsid, "2012-01-01", "2013-02-20")

   } 
    
}
\keyword{admin}