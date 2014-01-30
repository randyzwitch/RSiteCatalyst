\name{GetSegments}
\alias{GetSegments}
\title{GetSegments}
\usage{
GetSegments(reportsuite.ids)
}
\arguments{
  \item{reportsuite.ids}{report suite id (or list of report
  suite ids)}
}
\value{
List of valid segments
}
\description{
Gets a list of segments for the specified report suites.
Useful to find segment IDs for use in reporting helper
functions or JSON report definitions.
}
\examples{
segments <- GetSegments(c("your_prod_report_suite","your_dev_reportsuite"))
}

