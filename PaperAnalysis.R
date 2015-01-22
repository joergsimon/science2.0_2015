# savely install packages
# taken from
# http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
# and
# http://stackoverflow.com/questions/15155814/check-if-r-package-is-installed-then-load-library
pkgTest <- function(x) {
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("rAltmetric")
pkgTest("ropensci")
pkgTest("aRxiv")
pkgTest("devtools")
#if (!require("aRxiv")) {
#  install_github("rAltmetric", "ropensci", "ropensci/aRxiv")
#}

library(devtools)
library(aRxiv)
library(rAltmetric)
library(plyr)

# get the data from altmetrics.
# first check if anything exists with that arXiv id
# if nothing is found, check if the document has an doi,
# and if it has, try to get altmetrics with that
getAltmetricsData <- function(x) {
  arX <- x$id
  if (!is.null(arX)) {
    arX <- paste("arXiv:",arX,sep="")
    data <- altmetrics(arXiv = arX)
    if(!is.null(data)) {
      print("return arxiv data")
      return ( data )
    }
  }
  d <- x$doi
  if (is.null(d) || d == "") {
    print("return NULL, no doi")
    return ( NULL )
  }
  data <- altmetrics(doi = d)
  if(!is.null(data)) {
    print("return doi data")
  }
  return ( data )
}

# 'main' function triggering an almetric search
# and forward that result to further processing
processSearchstr <- function(searchString) {
  rec <<- arxiv_search(searchField, limit=2000)
  processResult(rec)
}

# process the result from the search in arXiv with 
# the rAltmetrics package to optain a list of
# altmetric objects
processResult <- function(rec) {
  raw_metrics <<- list()
  for (i in 1:nrow(rec)) {
    row <- rec[i,]
    print(paste("process row", i))
    data <- getAltmetricsData(row)
    if (!is.null(data)) {
      if (is.null(data$journal)) {
        data$journal <- ""
      }
      raw_metrics[[length(raw_metrics)+1]] <<- data
    }
  }
  processRawMetrics(raw_metrics)
}

# this method does not work in RStudio :/ 
# print the altmetrics objects in a plot
processRawMetrics <- function(raw_metrics) {
  for(i in 1:length(raw_metrics)) {
    print("--------------------------------------------------------------")
    f <- paste("/Volumes/Data/Dropbox/uni/Science2.0/T1/r1",paste(i, ".png",sep=""),sep="")
    print(f)
    png(filename=f, width = 480, height = 480, units = "px")
    plot(raw_metrics[[i]])
    Sys.sleep(0.5)
    dev.flush()
    dev.off()
  }
}

Sys.setenv(aRxiv_timeout = 100)

#searchField <- 'all:smartphone'
searchField <- 'all:pervasive'
#searchField <- 'all:Ubicomp OR all:MOBICOMP OR all:MobiHoc OR all:MSWiM OR all:WoWMoM OR all:Mobiquitous OR all:percom OR all:ICSPC'
processSearchstr(searchField)
