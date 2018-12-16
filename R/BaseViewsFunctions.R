library(dplyr, quietly = TRUE)
library(devtools, quietly = TRUE)

## Internal function
.get_packages <- function(binders){
  ## Description: Internal functions to get packages from binders  
  ## Args: binders - binder names to get packages of
  ## returns: vector of package names
  
  ## loading data frames consisting of views
  load("./R/sysdata.rda")
  pkgs <- NULL
  srcs <- NULL
  
  ## loading data frames consisting of views
  for(i in c(1:length(binders))){
    pkgs_to_add <- Topics.Views[Topics.Views$Binder %in% binders[i],c("Package","Source")]
    pkgs_to_add <- rbind(pkgs_to_add,User.Views[User.Views$Binder %in% binders[i],c("Package","Source")])
    
    ## Error handling
    if(dim(pkgs_to_add)[1] == 0){
      stop(paste0("Binder ",binders[i]," not found."))
    }
    
    pkgs <- rbind(pkgs,pkgs_to_add)
  }
  rm(Topics.Views, User.Views)
  
  return(unique(pkgs))
  
}


#' View Binders
#'
#' Function to see the binders of packages available and a summary for each. 
#' @usage view_binders()
#' @param compartment character vector, the compartment to view the binders in. Defaults to "all". 
#' - "all" to view all the binders
#' - "master" to view binders that came with the package
#' - "user" to view binders created by the user
#' @param search character string, regex style search string for binders with matches to words in the search string. Deafults to NULL in which case all the binders are shown. 
#' @examples view_binders(compartment="master") ## to view all the binders in the master list
#' @examples view_binders(compartment="master",binders="Machine Learning") ## to view a summary of just the Machine Learning Binder
#' 

view_binders <- function(compartment="all", search=NULL ){
  load("./R/sysdata.rda")
  compartment <- tolower(compartment)
  if(!(compartment %in% c("all","master","user"))){
    stop(paste0('Compartment ',compartment,' not found: Please enter either all, master or user'))
  }else if (compartment == "all"){
    views <- Topics.Views
    views <- rbind(views,User.Views)
  }else if(compartment == "master"){
    views <- Topics.Views
  }else if(compartment == "user"){
    views <- User.Views
  }
  
  if(length(search) > 0){
    binders <- views[grepl(paste0(search,"|",tolower(search)),views$Topic)| ## search in topics
                     grepl(paste0(search,"|",tolower(search)),views$Binder)| ## search in binders
                     grepl(paste0(search,"|",tolower(search)),views$Package),"Binder"] ## search in packages
    
    if(length(binders) == 0){
      # stop("Search found no results. Try changing the search query")
      return(NULL)
    }else{
      views <- views[views$Binder %in% binders,]
    }
  }
  
  views <- views %>% dplyr::group_by(Binder) %>% 
    summarise(packages=paste(Package,collapse=", "),
              topics=paste(names(sort(table(unlist(strsplit(paste(Topic,collapse = ", "),
                      ", "))),decreasing = TRUE))[1:min(3,
                      length(names(sort(table(unlist(strsplit(paste(Topic,collapse = ", "),
                              ", "))),decreasing = TRUE))))],collapse=", "),
              N=n()) 
  ## really ugly code to display topics
                                      
              
  
  rm(Topics.Views,User.Views)
  # print.data.frame(views)
  return(views)
}


#' Install Binders
#'
#' Function to install the packages in the binders. 
#' @usage install_binders(binders)
#' @param binders The binder to install 
#' @examples install_binders("MLM")
#' 
#' install_binders()

install_binders <- function(binders){
  pkgs <- .get_packages(binders)
  
  ## from CRAN
  pkgs_cran <- pkgs$Package[pkgs$Source == "CRAN"]
  install.packages(pkgs_cran,verbose=FALSE)
  
  ## external sources (github?)
  pkgs_ext <- pkgs[!(pkgs$Source == "CRAN"),]
  
  for(i in c(1:length(pkgs_ext))){
    tryCatch(
      {
        devtools::install_github(repo=pkgs_ext$Source[i])      
      },
      error=function(cond){
        stop(paste(pkgs_ext$Package[i]," not found in GitHub source ",pkgs_ext$Source[i],
                   ".Currently only installs via GitHub supported. Other sources will be added soon!"))
      }
    )
   
  }
  
  
  
}

#' Uninstall Binders
#'
#' Function to uninstall the packages in the binder(s). 
#' @usage uninstall_binders(binders)
#' @param binders The binder(s) to uninstall 
#' @examples uninstall_binders("MLM")
#' 

uninstall_binders <- function(binders){
  pkgs <- .get_packages(binders)
  remove.packages(pkgs)
}


#' Load Binders
#'
#' Function to load the packages in a binder(s). 
#' @usage load_binders(binders)
#' @param binders character vector of binder names. The binder(s) whose packages are to be loaded into the environment.  
#' @examples load_binders("MLM")
#' 

load_binders <- function(binders){
  pkgs <- .get_packages(binders)
  
  for(i in c(1:dim(pkgs)[1])){
    tryCatch(
      {
        library(pkgs$Package[i],character.only = TRUE)      
      },
      error=function(cond){
        if (pkgs$Source[i] == "CRAN"){
          install.packages(pkgs$Package[i])
          library(pkgs$Package[i],character.only = TRUE)        
        }else{
          devtools::install_github(repo=pkgs$Source[i])
          library(pkgs$Package[i],character.only = TRUE)        
        }
      } 
    )
  }
}

