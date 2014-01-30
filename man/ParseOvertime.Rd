\name{ParseOvertime}
\alias{ParseOvertime}
\title{ParseOvertime}
\usage{
ParseOvertime(report.data)
}
\arguments{
  \item{report.data}{jsonlite formatted data frame of
  report data returned from the API}
}
\value{
Formatted data frame
}
\description{
Internal Function - Parses an overtime report returned from
the API
}
\seealso{
Other internal: \code{\link{ApiRequest}};
\code{\link{BuildHeader}};
\code{\link{BuildInnerBreakdownsRecursively}};
\code{\link{ParseFallout}}; \code{\link{ParsePathing}};
\code{\link{ParseRanked}}; \code{\link{ParseTrended}}
}

