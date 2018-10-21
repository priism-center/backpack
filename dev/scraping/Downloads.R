## setup ## 
library(devtools)
#install_github("metacran/cranlogs")
library(cranlogs)
library(plyr)
library(tidyverse)

## load In-Views dataset ##

data <- read.csv("InViews.csv")
data <- data[-1]

## For each View see which package has the most downloads ## 

#Function 

Download <- function(ds,filword){
  x <- filter(ds, InViews == filword)
  x <- as.character(x[,"Package"])
  x <- cran_downloads(from = "2017-01-01", to = "2017-12-31",  package = x)
  x <- x %>%
    group_by(package) %>%
    summarise(Download2017 = sum(count))
  return(x)
}

BayesianDownloads2017 <- Download(data, "Bayesian")
ClusterDownloads2017  <- Download(data, "Cluster")
EconDownloads2017  <- Download(data, "Econometrics")
ExpDesignDownloads2017 <- Download(data, "ExperimentalDesign")
GraphicsDownloads2017 <- Download(data, "Graphics")
HPLearning2017 <- Download(data, "HighPerformanceComputing")
MachineLearning2017 <- Download(data, "MachineLearning")
MetaAnalysis2017 <- Download(data, "MetaAnalysis")
NLP2017 <- Download(data, "NaturalLanguageProcessing")
OS2017 <- Download(data, "OfficialStatistics")
Psych2017 <- Download(data, "Psychometrics")
RR2017 <- Download(data, "ReproducibleResearch")
Robust2017<- Download(data, "Robust")
Spatial2017 <- Download(data, "Spatial")
SpatioTemp2017 <- Download(data,"SpatioTemporal")
TS2017 <- Download(data, "TimeSeries")

Downloads2017 <- rbind(BayesianDownloads2017, ClusterDownloads2017, EconDownloads2017, ExpDesignDownloads2017, GraphicsDownloads2017,
                       HPLearning2017, MachineLearning2017, MetaAnalysis2017, NLP2017, OS2017, Psych2017, RR2017,
                       Robust2017, Spatial2017, SpatioTemp2017, TS2017)

Downloads2017 <- unique(Downloads2017)

Downloads2017$InViews <- data$InViews[match(Downloads2017$package, data$Package)]

write.csv(Downloads2017, "Downloads2017.csv")
