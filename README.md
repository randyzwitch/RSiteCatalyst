#RSiteCatalyst

R package to access Adobe Analytics REST Reporting API. 

This package is meant as an "analyst's toolbox" of functions to get digital analytics data from Adobe Analytics into R for more advanced reporting and analysis. While there are a few administrative functions that provide insight into Report Suite structure (such as available eVars/props/segments/etc), this package is not meant to be used for Report Suite Administration.

This package requires R 2.15.2 or greater. Also, due to the number of package dependencies, it's probably best to update all packages before installing RSiteCatalyst.  Package development was done (at minimum) on:

	R 2.15.3
	digest: 0.6.3
	RCurl: 1.95-3
	httr: 0.2
	rjson: 0.2.12
	plyr: 1.8
	stringr: 0.6.2

##News

2013-10-30: Version 1.2 changes thus far include:
- Removed RCurl package dependency
- Changed argument order for GetAdminConsoleLog to avoid error when date not passed
- Return proper numeric type for metric columns
- Some support for real-time reports


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

	install_github("RSiteCatalyst", "randyzwitch", ref = "version_1_2")
	library(RSiteCatalyst)

##Authentication

The Adobe Analytics Reporting API uses a "username/shared secret" method for authentication. This is done via the `SCAuth` function:

	SCAuth("username:company", "shared_secret")
	
If your authentication is successful, you will receive a console message of "Authentication succeeded"
	
##Package documentation

A video has been created to explain the main functions of the RSiteCatalyst package. You can access this video at [randyzwitch.com](http://randyzwitch.com/rsitecatalyst/).
