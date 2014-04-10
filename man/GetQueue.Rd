\name{GetQueue}
\alias{GetQueue}
\title{GetQueue}
\usage{
GetQueue()
}
\value{
data frame with: reportID, type, queueTime, status,
priority, estimate, reportSuiteID, user
}
\description{
Gets the report queue
}
\examples{
\dontrun{
queue <- GetQueue()
}
}

