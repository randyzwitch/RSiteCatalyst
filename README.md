#RSiteCatalyst

R package to access Adobe (Omniture) SiteCatalyst REST Reporting API. 

This package is meant as an "analyst's toolbox" of functions to get digital analytics data from SiteCatalyst into R for more advanced reporting and analysis. While there are a few administrative functions that provide insight into Report Suite structure (such as available eVars/props/segments/etc), this package is not meant to be used for Report Suite Administration.

This package requires R 2.15.2 or greater. Also, due to the number of package dependencies, it's probably best to update all packages before installing RSiteCatalyst.  Package development was done on:

	R 2.15.3
	digest: 0.6.3
	RCurl: 1.95-3
	httr: 0.2
	rjson: 0.2.12
	plyr: 1.8
	stringr: 0.6.2

##News


2013-03-18:  Updated to version 0.5. Version change reflects logic change in `QueueTrended` function to remove bug that didn't allow for anything except 'page' element


##Installation


###Manually:

Since this package has not yet been submitted to CRAN, you will need to load this package using the devtools package from Hadley Wickham.

Install the [devtools](https://github.com/hadley/devtools) package:

  	install.packages("devtools")
	library(devtools)

And then run the `install_github` command:

	install_github("RSiteCatalyst", "randyzwitch")
	library(RSiteCatalyst)

When loading the package using the `devtools` method, none of the package dependencies are loaded automatically like they would be if this package were loaded from [CRAN](http://cran.r-project.org/). Thus, depending on your current R environment, you may need to install digest, RCurl, httr, rjson, plyr, or stringr before installing RSiteCatalyst.

##Authentication

The Adobe Reporting API uses a "username/shared secret" method for authentication. This is done via the `SCAuth` function:

	SCAuth("username:company", "shared_secret")
	
To test that your credentials are working correctly, use the `GetTokenCount` function to see if an answer returns.

	GetTokenCount()
