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
Get Traffic Variables (props) Associated with a Report
Suite(s). Useful to audit or document a report suite or
company in Adobe Analytics.
}
\details{
This function requires having a character vector with one
or more valid Report Suites specified.
}
\examples{
\dontrun{
props <- GetProps("your_report_suite")

props2 <- GetProps(report_suites$rsid)
}
}

