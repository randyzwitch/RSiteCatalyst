\name{QueueOvertime}
\alias{QueueOvertime}
\title{
    Run a QueueOvertime Report
}
\description{
    A QueueOvertime report is a report where the only granularity allowed is time. This report allows for a single report suite, time granularity, multiple metrics, and a single segment. It is similar to the "Key Metrics" report or a Custom Event report within the SiteCatalyst interface.
}
\usage{
    QueueOvertime(reportSuiteID, dateFrom, dateTo, metrics, dateGranularity, 
                  segment_id, anomalyDetection, currentData)
}
\arguments{
    \item{reportSuiteID}{
        Report Suite ID
    }
    \item{dateFrom}{
        Report Start Date in "YYYY-MM-DD" format
    }
    \item{dateTo}{
        Report End Date in "YYYY-MM-DD" format. Should be less than or equal to 
        current date if using anomaly detection.
    }
    \item{metrics}{
        One or more metrics
    }
    \item{dateGranularity}{
        Optional. "Day", "Week", "Month", "Quarter" or "Year" (case-insensitive).
        If no granularity specified, a single row of data returned as sum of metrics for entire time period.
    }
    \item{segment_id}{
        Optional. If no segment_id is specified, metrics will be reported for 
        all visitors.
    }
    \item{anomalyDetection}{
    Optional. Use value of "1" to get anomaly detection results. Results only 
    returned by API for 'Day' granularity. 
    }
     \item{currentData}{
    Optional. Use value of "1" to get current data results. Only needed when 
    dateTo is greater than or equal to the current day.
    }
}
\details{

Because of the Reporting API structure, this function first requests the report, 
then checks the reporting queue to see if the report is completed, and when the 
report returns as "done" pulls the report from the API. This checking process can
occur up to 120 times, with a 5 second delay between status checks. If the report
does not return as "done" after 10 minutes, the function returns an error.
   
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

#Daily granularity for Loyal_Visitors segment (all arguments used)
loyal_visitors_feb_daily <- 
QueueOvertime("keystonerandy", "2013-02-01", "2013-07-28",
metrics = c("pageviews", "visits", "event2"), "day", "Loyal_Visitors", "1")


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