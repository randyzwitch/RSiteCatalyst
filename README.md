RSiteCatalyst
=============

R package to access Adobe (Omniture) SiteCatalyst Reporting API

This package is meant as an "analyst's toolbox" of functions to get digital analytics data from SiteCatalyst into R for more advanced reporting and analysis. While there are a few administrative functions that provide insight into Report Suite structure (such as available eVars/props/segments/etc), this package is not meant to be used for Report Suite Administration.


News
=============

Initial upload


Installation
=============

Manually:

Since this package has not yet been submitted to CRAN, you will need to load this package using the devtools

Install the [devtools](https://github.com/hadley/devtools) package:

  install.packages("devtools")
	library(devtools)

And then run the `install_github` command:

	install_github("RSiteCatalyst", "randyzwitch")
	library(RSiteCatalyst)
  

Authentication
=============

