\name{QueueOvertime}
\alias{QueueOvertime}
\title{Run a QueueOvertime Report}
\usage{
QueueOvertime(reportSuiteID, dateFrom, dateTo, metrics, dateGranularity = "",
  segment_id = "", anomalyDetection = "", currentData = "",
  maxTries = 120, waitTime = 5)
}
\arguments{
  \item{reportSuiteID}{Report Suite ID}

  \item{dateFrom}{Report Start Date in "YYYY-MM-DD" format}

  \item{dateTo}{Report End Date in "YYYY-MM-DD" format.
  Should be less than or equal to current date if using
  anomaly detection.}

  \item{metrics}{One or more metrics}

  \item{dateGranularity}{Optional. "Day", "Week", "Month",
  "Quarter" or "Year" (case-insensitive).  If no
  granularity specified, a single row of data returned as
  sum of metrics for entire time period.}

  \item{segment_id}{Optional. If no segment_id is
  specified, metrics will be reported for all visitors.}

  \item{anomalyDetection}{Optional. Use value of "1" to get
  anomaly detection results. Results only returned by API
  for 'Day' granularity.}

  \item{currentData}{Optional. Use value of "1" to get
  current data results. Only needed when dateTo is greater
  than or equal to the current day.}

  \item{maxTries}{Optional. Provide integer value for the
  max number of API attempts you want to try retrieve the
  report before function errors out. Defaults to 120.}

  \item{waitTime}{Optional. Provide integer value for the
  number of seconds between tries to API to try retrieve
  the report. Defaults to 5 seconds.}
}
\value{
Data Frame
}
\description{
A QueueOvertime report is a report where the only
granularity allowed is time. This report allows for a
single report suite, time granularity, multiple metrics,
and a single segment. It is similar to the "Key Metrics"
report or a Custom Event report within the SiteCatalyst
interface.
}
\details{
Because of the Reporting API structure, this function first
requests the report, then checks the reporting queue to see
if the report is completed, and when the report returns as
"done" pulls the report from the API. This checking process
will occur up to the specified number of times (default
120), with a delay between status checks (default 5
seconds). If the report does not return as "done" after the
number of tries have completed, the function will return an
error message.
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
\seealso{
\code{\link{GetAvailableMetrics}} \cr
\code{\link{GetSegments}}
}
\keyword{QueueOvertime}

