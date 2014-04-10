\name{QueueRanked}
\alias{QueueRanked}
\title{Run a QueueRanked Report}
\usage{
QueueRanked(reportSuiteID, dateFrom, dateTo, metrics, elements, top = "",
  startingWith = "", segment_id = "", selected = "", currentData = "",
  searchType = "", searchKW = "", maxTries = 120, waitTime = 5)
}
\arguments{
  \item{reportSuiteID}{Report Suite ID}

  \item{dateFrom}{Report Start Date in "YYYY-MM-DD" format}

  \item{dateTo}{Report End Date in "YYYY-MM-DD" format}

  \item{metrics}{The metric(s) you want in the report}

  \item{elements}{The element(s) (page, browser, eVar,
  prop) for the report to be broken down by.}

  \item{top}{How many results you want trended. Used in
  combination with "startingWith". Not used if "selected"
  argument used.}

  \item{startingWith}{The first ranked number you want in
  the report. Used in combination with "top". Not used if
  "selected" argument used.}

  \item{selected}{List of selected values, such as specific
  pages or eVar values. Not used if "top" and
  "startingWith" arguments are used}

  \item{segment_id}{Optional. If no segment_id is
  specified, metrics will be reported for all visitors.}

  \item{currentData}{Optional. Use value of "1" to get
  current data results. Only needed when dateTo is greater
  than or equal to the current day.}

  \item{searchType}{Optional. An enumerated list of boolean
  values used to link multiple search terms in a report
  search. Takes values of "AND", "OR" or "NOT".}

  \item{searchKW}{Optional. A list of keywords to include
  or exclude from the search, based on the searchType.
  Keyword values can also leverage the following special
  characters: '*' (Wild card), '^' (Starts With), '$' (Ends
  With). "Top" argument required when using regex
  functionality.}

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
A QueueRanked report is a report that shows the ranking of
values for one or more elements relative to a metric,
aggregated over the time period selected.
}
\details{
The QueueRanked function returns a data frame equivalent to
pulling a Ranked report in SiteCatalyst. Correlations &
Sub-Relations are supported.

Because of the Reporting API structure, this function first
requests the report, then checks the reporting queue to see
if the report is completed, and when the report returns as
"done" pulls the report from the API. This checking process
will occur up to the specified number of times (default
120), with a delay between status checks (default 5
seconds). If the report does not return as "done" after the
number of tries have completed, the function will return an
error message.

Note: Because of the multiple argument types ("top" and
"startingWith" OR "selected"), keyword arguments are
generally needed towards the end of the function call
instead of just positional arguments.
}
\examples{
\dontrun{

#Top 100 pages viewed/visits for Loyal_Visitors segment, broken down by browser
(correlation/subrelation)
top100_pages <-
QueueRanked("keystonerandy", "2013-02-13","2013-02-28", c("pageviews"), c('page', 'browser'),
top= "100", startingWith= "1", segment_id= "Loyal_Visitors")


#Get just the elements you want from the ranked list, instead of "Top"
single_pages <-
QueueRanked("keystonerandy", "2013-02-13","2013-02-19", c("pageviews", "visits"),'page',
selected = c("http://randyzwitch.com", "http://randyzwitch.com/about"))

   }
}
\seealso{
\code{\link{GetAvailableMetrics}} \cr
\code{\link{GetSegments}} \cr
\code{\link{GetAvailableElements}}
}
\keyword{QueueRanked}

