##### Master Functions #####


FullScrape <- function(URL, baseURL){
  
  suppressPackageStartupMessages(library(plyr))
  suppressPackageStartupMessages(library(purrr))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(XML))
  suppressPackageStartupMessages(library(rvest))
  suppressPackageStartupMessages(library(RCurl))

## Pull in list of Packages from CTV 
 
    Packages <-URL %>%
    read_html()%>%
    html_nodes(xpath = "/html/body/ul[1]/li")  %>% #unique xpath to pull in all individually
    html_text()
    
    Packages <- data.frame(Packages)
    names(Packages) <- "Packages"
    Packages$Packages <- as.character(Packages$Packages)


## Now we will construct URLs for each package
    URLs <- paste(baseURL, Packages$Packages, "/index.html", sep = "") #makes URLs that you can navigate to
    URLs <- data.frame(URLs)
    URLs$URLs <- as.character(URLs$URLs)
    Packages <- cbind(Packages, URLs)

## Loop to grab HTML of each URL (1 per package)
    mylist.names <- Packages$Packages
    HTMLs <- as.list(rep(NA, length(mylist.names)))
    names(HTMLs) <- mylist.names

    for(i in 1:nrow(URLs)){
    HTMLs[[i]] <- getURL(URLs$URLs[i])
    }
#return(list(Packages, HTMLs))
#save(InitialScrape, file = "InitialScrape.rda")  

    ###HTML Parse### 
  
    #Scrape Package Descriptions 
    
    mylist.names <- Packages$Packages
    PackageDesc <- as.list(rep(NA, length(mylist.names)))
    names(PackageDesc) <- mylist.names

    for(i in 1:length(HTMLs)){
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
    
    PackageDesc<- map_df(PackageDesc, ~as.data.frame(.x), .id="Package")
    names(PackageDesc) <- c("Package","PackageDescription")

    # Drop "Your browser sent a request that this server could not understand." - these are for CORE Packages 
    
    PackageDesc$PackageDescription <- as.character(PackageDesc$PackageDescription)
    PackageDesc <- PackageDesc %>%
    filter(PackageDescription != "Your browser sent a request that this server could not understand.")

    #Scrape Package Summaries
    mylist.names <- Packages$Packages
    PackageSummaries <- as.list(rep(NA, length(mylist.names)))
    names(PackageSummaries) <- mylist.names

    for(i in 1:length(HTMLs)){
    PackageSummaries[[i]] <-HTMLs[[i]] %>%
    read_html()%>%
    html_nodes(xpath = "//table[@summary][1]") %>%
    html_table() %>%
    data.frame(stringsAsFactors = FALSE)
    }

    #Remove empty dataframes from the list (Core Packages)
    PackageSummaries <-PackageSummaries[sapply(PackageSummaries, function(x) dim(x)[1]) > 0]

    #Convert dataframes to useable format 
    for(i in 1:length(PackageSummaries)){
    PackageSummaries[[i]]<-data.frame(split(PackageSummaries[[i]][1:nrow(PackageSummaries[[i]]), 2],PackageSummaries[[i]][1:nrow(PackageSummaries[[i]]),1]))
    } 

    #Unified dataframe
    PackageSummaries<- rbind.fill(PackageSummaries)


    #Scrape Reverse Dependencies
    mylist.names <- Packages$Packages
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

    ## Create Base Table 
    Base <-cbind(PackageDesc, PackageSummaries)
    Base <- left_join(Base, ReverseDepends)
    

return(list(Packages, HTMLs, PackageDesc, PackageSummaries, ReverseDepends, Base))    
}

#save(FullScrape, file = "FullScrape.rda")

