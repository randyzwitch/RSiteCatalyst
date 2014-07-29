RSiteCatalyst
===============

R client library for the Adobe Analytics 1.4 API. RSiteCatalyst v1.4 is still in development. Use at your own risk during the development staage, and please submit bug reports and/or pull requests for any issues!

Once finalized, full documentation will be created and posted at the following location:
[http://randyzwitch.github.io/RSiteCatalyst/](http://randyzwitch.github.io/RSiteCatalyst/)

## Installation
RSiteCatalyst v1.4 is in development and is not yet on CRAN. You will need to install directly from GitHub.

<<<<<<< HEAD
```R
install.packages("devtools")
library(devtools)
install_github("RSiteCatalyst", "randyzwitch", ref="version_1_4")
=======
	R 2.15.3
	digest: 0.6.3
	base64enc: 0.1-1
	httr: 0.2
	rjson: 0.2.12
	plyr: 1.8
	stringr: 0.6.2
>>>>>>> master

library(RSiteCatalyst)
```

<<<<<<< HEAD
You may also need to install other packages that _RSiteCatalyst v1.4_ depends on.
=======
2014-04-12: Version 1.3.3 submitted to CRAN
- Additional bug fixes, improvement to error reporting to use API error messages. 

**NOTE: Version 1.3.3 will likely be the last update for this codebase; Adobe has released version 1.4 of the API, and newer versions of RSiteCatalyst will be updated against the newer version of the API.**

2014-03-23: Version 1.3.2 submitted to CRAN
- Fixed issues arising from upgrading to httr 0.3. Package behavior should be identical using v0.2 or v0.3 of httr.

2014-02-20: Version 1.3.1 submitted to CRAN
- Fixed NAMESPACE issue to export real-time functions

2014-02-03: Version 1.3 submitted to CRAN

Changes in version 1.3 include
- Fixed validate flag in JSON request to work correctly
- Allow for variable API request timing in Queue functions
- Search via regex functionality in QueueRanked/QueueTrended functions
- Support for Realtime API reports: Overtime and one-element Ranked report
- Deprecated GetAdminConsoleLog

2013-11-04: Version 1.2 submitted to CRAN
>>>>>>> master

* [jsonlite](http://cran.r-project.org/web/packages/jsonlite/) (>=0.9.5)
* [plyr](http://cran.r-project.org/web/packages/plyr/) (>=1.8.1)
* [httr](http://cran.r-project.org/web/packages/httr/) (>= 0.3)
* [stringr](http://cran.r-project.org/web/packages/stringr/) (>=0.6.2)
* [digest](http://cran.r-project.org/web/packages/digest/)
* [base64enc](http://cran.r-project.org/web/packages/base64enc/)

```
install.packages(c("jsonlite","plyr","httr","stringr","digest","base64enc))
```

## Authorisation
Authorisation can be done using the legacy auth method (username:company + shared secret), or using the newer OAUTH method.
Either is fine, but ultimately you should move towards using the OAUTH method, as the legacy auth method is deprecated.

The OAUTH method is not universally available for all Adobe Analytics accounts, so legacy auth remains the default.

##### Using legacy auth (web service credentials)
This auth method is pretty straight-forward. You will simply need your username, and your shared secret, which you can retrieve from your account settings page in the Adobe Analytics web interface.

```
SCAuth("your_username:your_company", "your_shared_secret")
```

##### Using OAUTH
First you will need to create an application in the [Adobe Dev Center](https://developer.omniture.com/en_US/devcenter). The application name can be whatever you want. The redirect URI should be left blank.

This will provide you with a identifier and secret that you can use to access the Adobe Analytics API.

```
SCAuth("your_identifier", 
      "your_secret", 
      "your company")
```

## Running Reports
Once you've authorised, reports can be queued and retrieved using the helper libraries for each report type, or by using raw JSON report definitions.

#### Running a report using a JSON definition
The following code defines a JSON report description, and runs it. As no date granularity is specified, it will return a ranked report.

```
report.desc <- '{ "reportDescription" : { 
"dateFrom" : "2014-01-01", 
"dateTo" : "2014-11-07", 
"reportSuiteID" : "your_report_suite", 
"metrics" : [ { "id" : "pageviews" } ], 
"elements" : [ { "id" : "page" } ]
} }'

report.data <- JsonQueueReport(desc)
```

This is the same report description, but with daily date granularity, which will return a trended report.

```
report.desc <- '{ "reportDescription" : { 
"dateFrom" : "2014-01-01", 
"dateTo" : "2014-11-07", 
"dateGranularity" : "day", 
"reportSuiteID" : "your_report_suite", 
"metrics" : [ { "id" : "pageviews" } ], 
"elements" : [ { "id" : "page" } ]
} }'

report.data <- JsonQueueReport(desc)
```

#### Using the report helper functions
RSiteCatalyst v1.4 has helper functions that make it easier to generate all report types (ranked, overtime, trended, pathing, fallout). These take parameters in R, convert them to JSON, then call JsonQueueReport. _RSiteCatalyst v1.4_ helper functions do not yet support inline segmentation or search, so if you want to use that functionality, you will need to use JsonQueueReport directly.

#### QueueOvertime
Returns an overtime report. This is similar to the key metrics report, in that the only granularity allowed is time. 

QueueOvertime requires a start and end date, a reportsuite ID, and a character vector of metrics.

```
date.from <- "2014-01-01"
date.to <- "2013-01-07"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews")

report.data <- QueueOvertime(reportsuite.id, date.from, date.to, metrics)
```

You may also wish to set any of the 5 optional named parameters.

```
date.from <- "2014-01-01"
date.to <- "2014-01-07"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews")
date.granularity <- "hour"
segment.id <- "Visit_Natural_Search"
anomaly.detection <- TRUE
data.current <- TRUE
expedite <- TRUE

report.data <- QueueOvertime(reportsuite.id, date.from, date.to, metrics,date.granularity=date.granularity,segment.id=segment.id,anomaly.detection=anomaly.detection,data.current=data.current,expedite=expedite)
```

#### QueueRanked
Returns a ranked report. This is an ordered list of elements and associated metrics with no time granularity.

QueueRanked requires a start and end date, a reportsuite ID, a character vector of elements and a character vector of metrics.

```
date.from <- "2014-01-01"
date.to <- "2014-01-07"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews","event10")
elements <- c("page","geoCountry","geoCity")

report.data <- QueueRanked(reportsuite.id, date.from, date.to, metrics, elements)
```

You may also wish to set any of the 6 optional named parameters. While you can specify more than one element with _selected_, at this point, the 1.4 API only supports this for the first element specified.

```
date.from <- "2014-01-01"
date.to <- "2013-01-07"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews","event10","event10")
elements <- c("page","geoCountry","geoCity")
top <- 100
start <- 100
selected <- list(page=c("Home","Search","About"))
segment.id <- "dw:12345"
data.current <- TRUE
expedite <- TRUE

report.data <- QueueRanked(reportsuite.id, date.from, date.to, metrics,elements,top=top,start=start,selected=selected,segment.id=segment.id,data.current=data.current,expedit=expedite)
```

#### QueueTrended
Returns a trended report. This is an ordered list of elements and associated metrics with time granularity.

QueueTrended requires a start and end date, a reportsuite ID, a character vector of elements and a character vector of metrics.

```
date.from <- "2014-01-01"
date.to <- "2014-01-07"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews","event10")
elements <- c("page","geoCountry","geoCity")

report.data <- QueueTrended(reportsuite.id, date.from, date.to, metrics, elements)
```

You may also wish to set any of the 7 optional named parameters. As with _QueueRanked_ the 1.4 API only supports _selected_ for the first element specified.

```
date.from <- "2014-01-01"
date.to <- "2013-01-07"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews","event10")
elements <- c("page","geoCountry","geoCity")
top <- 100
start <- 100
selected <- list(page=c("Home","Search","About"))
date.granularity <- "hour"
segment.id <- "dw:12345"
data.current <- TRUE
expedite <- TRUE

report.data <- QueueTrended(reportsuite.id, date.from, date.to, metrics,elements,top=top,start=start,selected=selected,segment.id=segment.id,data.current=data.current,expedit=expedite)
```

#### QueuePathing
Returns a pathing report. This is an ordered list of paths matching the specified pattern.

QueuePathing requires a start and end date, a reportsuite ID, a single element, a single metric and a pattern of element values that defined the path.

```
date.from <- "2014-01-01"
date.to <- "2014-01-07"
reportsuite.id <- "your_report_suite"
metric <- "pageviews"
element <- "page"
pattern <- c("Home",":::anything:::",":::anything:::")

report.data <- QueuePathing(reportsuite.id, date.from, date.to, metric, element, pattern)
```

#### QueueFallout
Returns a fallout report. This is a pathed list of elements, with fallout values for each.

QueuePathing requires a start and end date, a reportsuite ID, a single element, a character vector of metrics and a character vector of element values that defined the checkpoints.

```
date.from <- "2014-01-01"
date.to <- "2014-01-07"
reportsuite.id <- "your_report_suite"
metrics <- c("visits","uniquevisitors","pageviews","event10")
element <- "page"
checkpoints <- c("Home","Contact","ThankYou")

report.data <- QueuePathing(reportsuite.id, date.from, date.to, metrics, element, checkpoints)
```

## Understanding the Available Data
Using the API, you can retrieve the setup of your report suite and view definitions for evars and sprops, success events, report suites and segments.

#### GetElements
Gets valid elements for a report suite for the current user. This list is restricted by optionally specified existing elements, existing metrics and date granularity.

```
elements.valid <- GetElements("your_report_suite",metrics=c('visitors','pageviews'),elements=c('page','geoCountry'),date.granularity='day')
```

#### GetMetrics
Gets valid metrics for a report suite for the current user. This list is restricted by optionally specified existing elements, existing metrics and date granularity.

```
metrics.valid <- GetMetrics("your_report_suite",metrics=c('visitors','pageviews'),elements=c('page','geoCountry'),date.granularity='day')
```

#### GetEvars
Gets evar (conversion variable) definitions for the specified report suite(s). Useful to audit or document a report suite or company in Adobe Analytics.

```
evars <- GetEvars(c("your_prod_report_suite","your_dev_reportsuite"))
```

#### GetProps
Gets sprop (traffic variable) definitions for the specified report suite(s). Useful to audit or document a report suite or company in Adobe Analytics.

```
props <- GetProps(c("your_prod_report_suite","your_dev_reportsuite"))
```

#### GetSuccessEvents
Gets success event definitions for the specified report suite(s). Useful to audit or document a report suite or company in Adobe Analytics.

```
successevents <- GetSuccessEvents(c("your_prod_report_suite","your_dev_reportsuite"))
```

#### GetReportSuites
Gets all report suites for the company.

```
reportsuites <- GetReportSuites()
```

#### GetSegments
Gets a list of segments for the specified report suites. Useful to find segment IDs for use in reporting helper functions or JSON report definitions.

```
segments <- GetSegments(c("your_prod_report_suite","your_dev_reportsuite"))
```
