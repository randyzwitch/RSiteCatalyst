#RSiteCatalyst

R package to access Adobe (Omniture) SiteCatalyst REST Reporting API. Requires R 2.15.2 or greater.

This package is meant as an "analyst's toolbox" of functions to get digital analytics data from SiteCatalyst into R for more advanced reporting and analysis. While there are a few administrative functions that provide insight into Report Suite structure (such as available eVars/props/segments/etc), this package is not meant to be used for Report Suite Administration.


##News


Initial upload


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

	SCAuth("username:company", "shared_secret", datacenter)
	
To obtain your username and shared secret, you need to be in the "Web Services Access" group for the SiteCatalyst Report Suite you are trying to access. This can be done via the SiteCatalyst Admin panel.

In order to figure out which data center you need for the third argument, there are several approaches. The fastest approach is likely just to substitute a value of "1" into the `SCAuth` function, then call the `GetTokenCount` function:

	SCAuth("username:company", "shared_secret", 1)
	GetTokenCount()
	
If using a value of "1" then calling the `GetTokenCount` function returns an answer, then that's your data center. If not, repeat using values of "2", "3", or "4" until `GetTokenCount` returns an answer.

If you have the desire to be more elegant in finding your data center, you can access the [Adobe documentation](http://microsite.omniture.com/t2/help/en_US/home/index.html#kb-determining-data-center) or use the [Adobe API Explorer](https://developer.omniture.com/en_US/get-started/api-explorer).
	
