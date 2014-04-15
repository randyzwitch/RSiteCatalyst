\name{GetProps}
\alias{GetProps}
\title{Get Traffic Variables (props) Associated with a Report Suite}
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
GetProps
}
\details{
Get Traffic Variables (props) Associated with a Report
Suite(s). Useful to audit or document a report suite or
company in Adobe Analytics.
}
\examples{
\dontrun{
props <- GetProps(c("your_prod_report_suite","your_dev_reportsuite"))
}
}

