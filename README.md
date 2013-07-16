#RSiteCatalyst

R package to access Adobe (Omniture) SiteCatalyst REST Reporting API. 

This package is meant as an "analyst's toolbox" of functions to get digital analytics data from SiteCatalyst into R for more advanced reporting and analysis. While there are a few administrative functions that provide insight into Report Suite structure (such as available eVars/props/segments/etc), this package is not meant to be used for Report Suite Administration.

This package requires R 2.15.2 or greater. Also, due to the number of package dependencies, it's probably best to update all packages before installing RSiteCatalyst.  Package development was done (at minimum) on:

	R 2.15.3
	digest: 0.6.3
	RCurl: 1.95-3
	httr: 0.2
	rjson: 0.2.12
	plyr: 1.8
	stringr: 0.6.2

##News

2013-06-17:  Version 1.1 in development to support correlations/sub-relations

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

	install_github("RSiteCatalyst", "randyzwitch", ref = "add_correlations")
	library(RSiteCatalyst)

##Authentication

The Adobe Reporting API uses a "username/shared secret" method for authentication. This is done via the `SCAuth` function:

	SCAuth("username:company", "shared_secret")
	
To test that your credentials are working correctly, use the `GetTokenCount` function to see if an answer returns.

	GetTokenCount()
	
##Package documentation

A video has been created to explain the main functions of the RSiteCatalyst package. You can access this video at [randyzwitch.com](http://randyzwitch.com/rsitecatalyst/).
