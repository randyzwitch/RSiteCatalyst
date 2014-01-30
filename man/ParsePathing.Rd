\name{ParsePathing}
\alias{ParsePathing}
\title{ParsePathing}
\usage{
ParsePathing(report.data)
}
\arguments{
  \item{report.data}{jsonlite formatted data frame of
  report data returned from the API}
}
\value{
Formatted data frame
}
\description{
Internal Function - Parses a pathing report returned from
the API
}
\seealso{
Other internal: \code{\link{ApiRequest}};
\code{\link{BuildHeader}};
\code{\link{BuildInnerBreakdownsRecursively}};
\code{\link{ParseFallout}}; \code{\link{ParseOvertime}};
\code{\link{ParseRanked}}; \code{\link{ParseTrended}}
}

