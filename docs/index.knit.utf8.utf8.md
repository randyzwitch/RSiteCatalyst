---
title: "RSiteCatalyst Documentation"
output: 
  html_document:
    highlight: tango
    toc: true
    toc_float:
      collapsed: false
---



## Introduction

You can render collections of R Markdown documents as a website using the `rmarkdown::render_site` function. This article describes the basics of creating websites with R Markdown and provides some examples which you can use as a starting point for your own websites.



The RStudio IDE also includes integrated support for developing R Markdown websites. These features are available in the current Preview Release of RStudio which you can install from here: [RStudio Preview Release](https://www.rstudio.com/products/rstudio/download/preview/).

## Getting Started

---

### Installation
The features described here are available only within the most recent version of the **rmarkdown** package (v0.9.6) which you can install from CRAN as follows:
```r
install.packages("rmarkdown", type = "source")
```

---

### Authentication
#### Legacy
#### OAuth
### Tips and Tricks

---

## Examples

---

## Contributing

---

### Issues / Feature Requests
### Submiting A Pull Request 
### Contributors

---

## Admin / Metadata Functions
### CancelReport [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetActivation [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com) 
### GetAxleStartDate [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetBaseCurrency [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetBaseURL [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetBookmarks [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetCalculatedMetrics [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetClassifications [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetCustomCalendar [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetDashboards [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetDataWarehouseDisplay [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetDefaultPage [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetDiscoverEnabled [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetEcommerce [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetElements [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetEvars [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetFeed [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetFeeds [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetFunctions [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetGeoSegmentation [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetGroups [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetInternalURLFilters [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetIPAddressExclusions [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetIPObfuscation [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetKeyVisitors [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetListVariables [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetLocalization [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetLogin [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetMarketingChannelExpiration [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetMarketingChannelRules [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetMarketingChannels [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetMetrics [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetMobileAppReporting [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetPaidSearchDetection [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetPermanentTraffic [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetPrivacySettings [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetProps [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetQueue [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetRealTimeReport [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetRealTimeSettings [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetReport [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetReportDescription [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetReportsByIds [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetReportSuites [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetScheduledSpike [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetSegments [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetSiteTitle [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetSuccessEvents [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetTemplate [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetTimeZone [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetTrackingServer [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetTransactionEnabled [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetUniqueVisitorVariable [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetVersionAccess [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### GetVideoSettings [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)

---

## Analytics Functions
### BuildClassificationValueSegment [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### BuildRealTimeReportStructure [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### QueueFallout [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### QueueOvertime [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### QueuePathing [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### QueueRanked [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### QueueSummary [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### QueueTrended [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### SaveRealTimeSettings [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### SCAuth [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
### SubmitJsonQueueReport [<span class="glyphicon glyphicon-link"</span>](http://randyzwitch.com)
