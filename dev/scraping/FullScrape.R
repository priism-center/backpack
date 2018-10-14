URLs <- list("https://cran.r-project.org/web/views/Bayesian.html",
             "https://cran.r-project.org/web/views/Cluster.html",
             "https://cran.r-project.org/web/views/Econometrics.html",
             "https://cran.r-project.org/web/views/ExperimentalDesign.html",
             "https://cran.r-project.org/web/views/Graphics.html",
             "https://cran.r-project.org/web/views/HighPerformanceComputing.html",
             "https://cran.r-project.org/web/views/MachineLearning.html", 
             "https://cran.r-project.org/web/views/MetaAnalysis.html",
             "https://cran.r-project.org/web/views/NaturalLanguageProcessing.html",
             "https://cran.r-project.org/web/views/OfficialStatistics.html",
             "https://cran.r-project.org/web/views/Psychometrics.html",
             "https://cran.r-project.org/web/views/ReproducibleResearch.html",
             "https://cran.r-project.org/web/views/Robust.html",
             "https://cran.r-project.org/web/views/Spatial.html",
             "https://cran.r-project.org/web/views/SpatioTemporal.html",
             "https://cran.r-project.org/web/views/TimeSeries.html")

FullScrape <- function(URLs){
  suppressPackageStartupMessages(library(plyr))
  suppressPackageStartupMessages(library(purrr))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(XML))
  suppressPackageStartupMessages(library(rvest))
  suppressPackageStartupMessages(library(RCurl))
  suppressPackageStartupMessages(library(lubridate))
  
  ## Pull in list of Packages from CTV 
  Packages <- list()
  
  for (i in 1:length(URLs)){
    
    Packages[[i]] <-URLs[[i]] %>%
    read_html()%>%
    html_nodes(xpath = "/html/body/ul[1]/li")  %>% #unique xpath to pull in all individually
    html_text()
  
    }
  
  Packages <- unlist(Packages)
  Packages <- data.frame(Packages)

  Packages$Packages <- as.character(Packages$Packages)
  Packages$Packages <- gsub(pattern = " (core)", replacement = "",Packages$Packages, fixed = TRUE)
  Packages <- data.frame(unique(Packages$Packages))
  names(Packages)   <- "Package"


  Packages$URL <- paste("https://cran.r-project.org/web/packages/", Packages$Package, "/index.html", sep = "") 
  
  #Extract HTMLs 
  
  mylist.names <- Packages$Package
  HTMLs <- as.list(rep(NA, length(mylist.names)))
  names(HTMLs) <- mylist.names
  
  for(i in 1:nrow(Packages)){
    HTMLs[[i]] <- getURL(Packages$URL[i])
  }
  
  mylist.names <- Packages$Package
  PackageDesc <- as.list(rep(NA, length(mylist.names)))
  names(PackageDesc) <- mylist.names
  
  for(i in 1:length(PackageDesc)){
    PackageDesc[[i]] <-HTMLs[[i]] %>%
      read_html()%>%
      html_nodes(xpath = "/html/body/p[1]") %>%
      html_text()
  }
  
  #Clean Package Descriptions
  
  for(i in 1:length(PackageDesc)){
    PackageDesc[[i]] <- PackageDesc[[i]] %>%
      str_replace_all("\n", "") %>%
      str_replace_all("\\s+", " ")%>%
      str_replace_all("^\\s", "") %>%
      str_replace_all("\\s$", "")
  }
  
  #Make dataframe

  PackageDesc<- map_df(PackageDesc, ~as.data.frame(as.character(.x)), .id="Package")
  names(PackageDesc) <- c("Package","PackageDescription")


  Packages <- left_join(Packages, PackageDesc, by = "Package")

  #Scrape Package Summaries
  mylist.names <- Packages$Package
  PackageSummaries <- as.list(rep(NA, length(mylist.names)))
  names(PackageSummaries) <- mylist.names

  for(i in 1:length(HTMLs)){
    PackageSummaries[[i]] <-HTMLs[[i]] %>%
      read_html()%>%
      html_nodes(xpath = "//table[@summary][1]") %>%
      html_table() %>%
      data.frame(stringsAsFactors = FALSE)
  }

  #Convert dataframes to useable format
  for(i in 1:length(PackageSummaries)){
    PackageSummaries[[i]]<-data.frame(split(PackageSummaries[[i]][1:nrow(PackageSummaries[[i]]), 2],PackageSummaries[[i]][1:nrow(PackageSummaries[[i]]),1]))
  }

  #Unified dataframe
  PackageSummaries<- rbind.fill(PackageSummaries)

  #cbind to Packages Base DF
  Packages <- cbind(Packages, PackageSummaries)
  
  #Scrape Reverse Dependencies
  mylist.names <- Packages$Package
  ReverseDepends <- as.list(rep(NA, length(mylist.names)))
  names(ReverseDepends) <- mylist.names

  for(i in 1:length(HTMLs)){
    ReverseDepends[[i]] <-HTMLs[[i]] %>%
      read_html()%>%
      html_nodes(xpath = "//tr/td[contains(text(), 'Reverse')]/ancestor::table") %>%
      html_table() %>%
      data.frame(stringsAsFactors = FALSE)
  }

  #Remove empty dataframes from the list (Core Packages)
  ReverseDepends <- ReverseDepends[sapply(ReverseDepends, function(x) dim(x)[1]) > 0]

  #Convert dataframes to useable format
  for(i in 1:length(ReverseDepends)){
    ReverseDepends[[i]]<-data.frame(split(ReverseDepends[[i]][1:nrow(ReverseDepends[[i]]), 2],ReverseDepends[[i]][1:nrow(ReverseDepends[[i]]),1]))
  }

  #Make into one usable dataframe
  ReverseDepends <- map_df(ReverseDepends, ~as.data.frame(.x), .id="Package")
  
  #Join Reverse Depends to base DF
  Packages <- left_join(Packages, ReverseDepends)
  
  #For each package add the most recent update date
  
  TimeUpdateURLs <- data.frame(Packages$Package,paste("https://cran.r-project.org/web/checks/check_results_", Packages$Package, ".html", sep = ""))
  names(TimeUpdateURLs) <- c("Package", "UpdateURL")
  
  mylist.names <- TimeUpdateURLs$Package
  UpdateHTMLs <- as.list(rep(NA, length(mylist.names)))
  names(UpdateHTMLs) <- mylist.names
  
  for(i in 1:nrow(TimeUpdateURLs)){
    UpdateHTMLs[[i]] <- getURL(TimeUpdateURLs$UpdateURL[i])
  }
  
  mylist.names <- Packages$Package
  Updated <- as.list(rep(NA, length(mylist.names)))
  names(Updated) <- mylist.names
  
  for(i in 1:length(UpdateHTMLs)){
    Updated[[i]] <-UpdateHTMLs[[i]] %>%
      read_html()%>%
      html_nodes(xpath = "/html/body/p[1]") %>%
      html_text() 
  }
  
  Updated <- map_df(Updated, ~as.data.frame(.x), .id="Package")
  names(Updated) <- c("Package", "LastUpdate")
  Updated$LastUpdate <- ymd_hms(Updated$LastUpdate)
  
  Packages <- left_join(Packages, Updated)
  return(Packages)
}

Packages <- FullScrape(URLs)
write.csv(Packages, "PackagesBase.csv")

## Take Package DF and Make Edge lists with: DependS, Enhances, Imports, InViews, LinkingTo, 
## ReverseDepends, ReverseEnhances, ReverseImports, ReverseLinkingTo, ReverseSuggests, Suggests 

MakeEdgeLists <- function(data){ 
  
  #Load libraries
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(tidyr))
  
  #Make Depends Table 
  Depends <- data %>%
    select(Package, Depends.) %>%
    mutate(Depends. = strsplit(as.character(Depends.), ",")) %>% 
    unnest(Depends.)
  
  names(Depends)[2] <- "Depends"
  Depends$Depends <- trimws(Depends$Depends)
  
  Depends <- Depends %>%
    filter(!is.na(Depends))
  
  #Make InViews Table
  InViews <- data %>%
    select(Package, In.views.) %>%
    mutate(In.views. = strsplit(as.character(In.views.), ","))%>%
    unnest(In.views.)
  
  names(InViews)[2] <- "InViews"
  InViews$InViews <- trimws(InViews$InViews)
  
  InViews <- InViews %>%
    filter(!is.na(InViews))
  
  #Make Imports Table
  
  Imports <- data %>%
    select(Package, Imports.) %>%
    mutate(Imports. = strsplit(as.character(Imports.), ","))%>%
    unnest(Imports.)
  
  names(Imports)[2] <- "Imports"
  Imports$Imports <- trimws(Imports$Imports)
  
  Imports <- Imports %>%
    filter(!is.na(Imports))
  
  #Make LinkingTo Table
  LinkingTo <- data %>%
    select(Package, LinkingTo.) %>%
    mutate(LinkingTo. = strsplit(as.character(LinkingTo.), ","))%>%
    unnest(LinkingTo.)
  
  names(LinkingTo)[2] <- "LinkingTo"
  LinkingTo$LinkingTo <- trimws(LinkingTo$LinkingTo)
  
  LinkingTo <- LinkingTo %>%
    filter(!is.na(LinkingTo))
  
  #Make Suggests Table
  Suggests <- data %>%
    select(Package, Suggests.) %>%
    mutate(Suggests. = strsplit(as.character(Suggests.), ","))%>%
    unnest(Suggests.)
  
  names(Suggests)[2] <- "Suggests"
  Suggests$Suggests <- trimws(Suggests$Suggests)
  
  Suggests <- Suggests %>%
    filter(!is.na(Suggests))
  
  #Make Enhances Table
  Enhances <- data %>%
    select(Package, Enhances.) %>%
    mutate(Enhances. = strsplit(as.character(Enhances.), ","))%>%
    unnest(Enhances.)
  
  names(Enhances)[2] <- "Enhances"
  Enhances$Enhances <- trimws(Enhances$Enhances)
  
  Enhances <- Enhances %>%
    filter(!is.na(Enhances))
  
  #Make ReverseDepends Table
  ReverseDepends <- data %>%
    select(Package, Reverse.depends.) %>%
    mutate(Reverse.depends. = strsplit(as.character(Reverse.depends.), ","))%>%
    unnest(Reverse.depends.)
  
  names(ReverseDepends)[2] <- "ReverseDepends"
  ReverseDepends$ReverseDepends <- trimws(ReverseDepends$ReverseDepends)
  
  ReverseDepends <-ReverseDepends %>%
    filter(!is.na(ReverseDepends))
  
  #Make ReverseImports Table
  ReverseImports <- data %>%
    select(Package, Reverse.imports.) %>%
    mutate(Reverse.imports. = strsplit(as.character(Reverse.imports.), ","))%>%
    unnest(Reverse.imports.)
  
  names(ReverseImports)[2] <- "ReverseImports"
  ReverseImports$ReverseImports <- trimws(ReverseImports$ReverseImports)
  
  ReverseImports <- ReverseImports %>%
    filter(!is.na(ReverseImports))
  
  #Make ReverseSuggests Table
  ReverseSuggests <- data%>%
    select(Package, Reverse.suggests.) %>%
    mutate(Reverse.suggests. = strsplit(as.character(Reverse.suggests.), ","))%>%
    unnest(Reverse.suggests.)
  
  names(ReverseSuggests)[2] <- "ReverseSuggests"
  ReverseSuggests$ReverseSuggests <- trimws(ReverseSuggests$ReverseSuggests)
  
  ReverseSuggests <- ReverseSuggests %>%
    filter(!is.na(ReverseSuggests))
  
  #Make ReverseLinkingTo Table
  ReverseLinkingTo <- data%>%
    select(Package, Reverse.linking.to.) %>%
    mutate(Reverse.linking.to. = strsplit(as.character(Reverse.linking.to.), ","))%>%
    unnest(Reverse.linking.to.)
  
  names(ReverseLinkingTo)[2] <- "ReverseLinkingTo"
  ReverseLinkingTo$ReverseLinkingTo <- trimws(ReverseLinkingTo$ReverseLinkingTo)
  
  ReverseLinkingTo <- ReverseLinkingTo%>%
    filter(!is.na(ReverseLinkingTo))
  
  #Make Reverse Enhances 
  ReverseEnhances <- data %>%
    select(Package, Reverse.enhances.) %>%
    mutate(Reverse.enhances. = strsplit(as.character(Reverse.enhances.), ","))%>%
    unnest(Reverse.enhances.)
  
  names(ReverseEnhances)[2] <- "ReverseEnhances"
  ReverseEnhances$ReverseEnhances <- trimws(ReverseEnhances$ReverseEnhances)
  
  ReverseEnhances <- ReverseEnhances%>%
    filter(!is.na(ReverseEnhances))
  
  #Return
  return(list(Depends, InViews, Imports, LinkingTo, Suggests, Enhances, ReverseDepends, ReverseImports,
              ReverseSuggests, ReverseLinkingTo, ReverseEnhances))
}

EdgeLists <- MakeEdgeLists(Packages)


