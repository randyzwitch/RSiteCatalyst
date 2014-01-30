\name{GetProps}
\alias{GetProps}
\title{GetProps}
\usage{
GetProps(reportsuite.ids)
}
\arguments{
  \item{reportsuite.ids}{report suite id (or list of report
  suite ids)}
}
\value{
List of valid props
}
\description{
Gets sprop (traffic variable) definitions for the specified
report suite(s). Useful to audit or document a report suite
or company in Adobe Analytics.
}
\examples{
props <- GetProps(c("your_prod_report_suite","your_dev_reportsuite"))
}

