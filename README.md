#RSiteCatalyst

R package to access Adobe Analytics REST Reporting API. 

This package is meant as an "analyst's toolbox" of functions to get digital analytics data from Adobe Analytics into R for more advanced reporting and analysis. While there are a few administrative functions that provide insight into Report Suite structure (such as available eVars/props/segments/etc), this package is not meant to be used for Report Suite Administration.

This package requires R 2.15.2 or greater. Also, due to the number of package dependencies, it's probably best to update all packages before installing RSiteCatalyst.  Package development was done (at minimum) on:

	R 2.15.3
	digest: 0.6.3
	base64enc: 0.1-1
	httr: 0.2
	rjson: 0.2.12
	plyr: 1.8
	stringr: 0.6.2

##News

2014-02-03: Version 1.3 submitted to CRAN

Changes in version 1.3 include
- Fixed validate flag in JSON request to work correctly
- Allow for variable API request timing in Queue functions
- Search via regex functionality in QueueRanked/QueueTrended functions
- Support for Realtime API reports: Overtime and one-element Ranked report
- Deprecated GetAdminConsoleLog

2013-11-04: Version 1.2 submitted to CRAN

Changes in version 1.2 include
- Removed RCurl package dependency
- Changed argument order for GetAdminConsoleLog to avoid error when date not passed
- Return proper numeric type for metric columns
- Fixed bug in GetEVars function
- Added validate:true flag to API to improve error reporting

Version 1.2 was developed on OSX Lion using R 3.0.2 & validated against OSX Lion, Ubuntu 12.04 LTS. Further validation against Windows 7 & Windows 8 is planned.

2013-08-11:  Version 1.1 submitted to CRAN (master branch in sync with CRAN submission).

Changes to version 1.1 include:
- Support for correlations/sub-relations
- Anomaly Detection
- Current Data
- Lower wait time between API calls/Extend number of API tries before report failure

2013-04-25:  [RSiteCatalyst](http://cran.r-project.org/web/packages/RSiteCatalyst/) is now available on CRAN.


##Installation

To install the stable version, use the standard package installation method

	install.packages("RSiteCatalyst")

###Manually:

To load this package from GitHub, use the following commands. Note, this should only be done if you want to test development versions of the package.

Install the [devtools](https://github.com/hadley/devtools) package:

  install.packages("devtools")
	library(devtools)

And then run the `install_github` command:

	install_github("RSiteCatalyst", "randyzwitch", ref = "master")
	library(RSiteCatalyst)

##Authentication

The Adobe Analytics Reporting API uses a "username/shared secret" method for authentication. This is done via the `SCAuth` function:

	SCAuth("username:company", "shared_secret")
	
If your authentication is successful, you will receive a console message of "Authentication succeeded"
	
##Package documentation

A video has been created to explain the main functions of the RSiteCatalyst package. You can access this video at [randyzwitch.com](http://randyzwitch.com/rsitecatalyst/).
