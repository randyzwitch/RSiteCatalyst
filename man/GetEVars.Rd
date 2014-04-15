\name{GetEvars}
\alias{GetEvars}
\title{Get Commerce Variables (eVars) Associated with a Report Suite}
\usage{
GetEvars(reportsuite.ids)
}
\arguments{
  \item{reportsuite.ids}{report suite id (or list of report
  suite ids)}
}
\value{
List of valid evars
}
\description{
GetEvars
}
\details{
Gets available report suite evars
}
\examples{
\dontrun{
evars <- GetEvars("reportsuite"your-report-suite")

evars <- GetEvars(report_suites$rsid)

}
}

