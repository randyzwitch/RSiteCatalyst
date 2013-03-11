\name{QueueOvertime}
\alias{QueueOvertime}
\title{
    Run a QueueOvertime Report
}
\description{
    A QueueOvertime report is a report where the only granularity allowed is time. This report allows for a single report suite, time granularity, multiple metrics, and a single segment. It is similar to the "Key Metrics" report or a Custom Event report within the SiteCatalyst interface.
}
\usage{
    QueueOvertime(reportSuiteID, dateFrom, dateTo, metrics, dateGranularity, segment_id)
}
\arguments{
    \item{reportSuiteID}{
        Report Suite ID
    }
    \item{dateFrom}{
        Report Start Date in "YYYY-MM-DD" format
    }
    \item{dateTo}{
        Report End Date in "YYYY-MM-DD" format
    }
    \item{metrics}{
        One or more metrics
    }
    \item{dateGranularity}{
        Optional. "Day", "Week", "Month", "Quarter" or "Year" (case-insensitive). If no granularity specified, a single row of data returned as sum of metrics for entire time period.
    }
    \item{segment_id}{
        Optional. If no segment_id is specified, metrics will be reported for all visitors.
    }
}
\details{

Because of the Reporting API structure, this function first requests the report, then checks the reporting queue to see if the report is completed, and when the report returns as "done" pulls the report from the API. This checking process can occur up to 10 times, with a 10 second delay between status checks. If the report does not return as "done" after 10 tries, the function returns an error.
   
}
\value{
    Data Frame
}

\seealso{
   \code{\link{GetAvailableMetrics}} \cr
   \code{\link{GetSegments}} 
     
}
\examples{
\dontrun{

#Hourly granularity for Loyal_Visitors segment (all arguments used)
loyal_visitors_feb_hourly <- 
QueueOvertime("keystonerandy", "2013-02-01", "2013-02-28",
metrics = c("pageviews", "visits", "event2"), "hour", "Loyal_Visitors")


#No granularity using empty string in dateGranularity position
loyal_visitors_feb_overall <- 
QueueOvertime("keystonerandy", "2013-02-01", "2013-02-28",
metrics = c("pageviews", "visits", "event2"), "", "Loyal_Visitors")


#Minimum number of arguments, single row containing sum of pageviews
pageviews_feb <- 
QueueOvertime("keystonerandy", "2013-02-01", "2013-02-28", "pageviews")
   }
}
\keyword{QueueOvertime}