\name{GetSuccessEvents}
\alias{GetSuccessEvents}
\title{GetSuccessEvents}
\usage{
GetSuccessEvents(reportsuite.ids)
}
\arguments{
  \item{reportsuite.ids}{report suite id (or list of report
  suite ids)}
}
\value{
List of valid successevents
}
\description{
Gets success event definitions for the specified report
suite(s). Useful to audit or document a report suite or
company in Adobe Analytics.
}
\examples{
successevents <- GetSuccessEvents(c("your_prod_report_suite","your_dev_reportsuite"))
}

