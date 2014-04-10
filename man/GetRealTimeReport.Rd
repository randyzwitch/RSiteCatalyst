\name{GetRealTimeReport}
\alias{GetRealTimeReport}
\title{Get Real-Time Report}
\usage{
GetRealTimeReport(report_suite, metrics, elements = c(), periodMinutes = 1,
  periodCount = 15, periodOffset = 0, algorithm = "most popular",
  algorithmArgument = "linear", firstRankPeriod = "0",
  floorSensitivity = 0.25)
}
\arguments{
  \item{report_suite}{Report Suite ID}

  \item{metrics}{Metric to include in the Realtime report.
  The metric must be configured previously using
  SaveRealTimeConfiguration}

  \item{elements}{A list of elements that breaks down
  (organizes) the metrics data in the report. Each element
  must be configured previously using
  SaveRealTimeConfiguration.

  If no elements are provided, an overtime report is
  generated for the provided metric.}

  \item{periodMinutes}{Number of minutes for one period.
  Default is 1.}

  \item{periodCount}{The number of periods of data to
  return. Default is 15.}

  \item{periodOffset}{Number of minutes before the current
  minute to run the report. If set to 10, the most recent
  result will be from ten minutes prior to the request.
  Default is 0.}

  \item{algorithm}{Type of dimensions to return, one of the
  following three values: 'gainers', 'losers', 'most
  popular'. Default is 'most popular'.}

  \item{algorithmArgument}{Specifies how to order the
  values for Most Popular, Gainers or Losers. Specify
  either percent, or linear. Default is linear.}

  \item{firstRankPeriod}{Computes the ranking of elements
  by considering the element's counts from the
  firstRankPeriod to the final period. With this argument
  you can rank from the first period (0) to periodCount - 1
  (most popular) or periodCount - 3 (gainers/losers) or
  anywhere in between. The firstRankPeriod is 0 based.}

  \item{floorSensitivity}{A factor between 0 and 1 that is
  used to cut off low-count items from percentage ranking.
  Relative only to gainers/losers by percent. Default is
  .25.}
}
\value{
Data Frame
}
\description{
Get real-time report
}
\details{
GetRealTimeReport returns a Data Frame with the results
from the real-time report specified. To find out valid
values for the inputs of this function, run
GetRealTimeConfiguration to get the current report setup.
To change the real-time report setup, use
SaveRealTimeConfiguration.

Currently, RSiteCatalyst only supports up to one element in
a realtime report. This limitation will be removed in a
future release.
}
\examples{
\dontrun{

  #Minimum example - Equivalent to Overtime report
  GetRealTimeReport("keystonerandy", "instances")

   }
}
\references{
See documentation for in-depth documentation:

https://developer.omniture.com/en_US/documentation/sitecatalyst-reporting/r-realtimereportdescription
}
\seealso{
\code{\link{GetRealTimeConfiguration}}\cr
\code{\link{SaveRealTimeConfiguration}}\cr
}
\keyword{GetRealTimeConfiguration}

