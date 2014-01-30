\name{ParseRanked}
\alias{ParseRanked}
\title{ParseRanked}
\usage{
ParseRanked(report.data)
}
\arguments{
  \item{report.data}{jsonlite formatted data frame of
  report data returned from the API}
}
\value{
Formatted data frame
}
\description{
Internal Function - Parses a ranked report returned from
the API
}
\seealso{
Other internal: \code{\link{ApiRequest}};
\code{\link{BuildHeader}};
\code{\link{BuildInnerBreakdownsRecursively}};
\code{\link{ParseFallout}}; \code{\link{ParseOvertime}};
\code{\link{ParsePathing}}; \code{\link{ParseTrended}}
}

