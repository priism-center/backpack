
.get_packages <- function(binders){
  ## Description: Internal functions to get packages from binders  
  ## Args: binders - binder names to get packages of
  ## returns: vector of package names
  
  ## loading data frames consisting of views
  load("./R/sysdata.rda")
  pkgs <- NULL
  
  ## loading data frames consisting of views
  for(i in c(1:length(binders))){
    pkgs_to_add <- Topics.Views$Package[Topics.Views$Topic %in% binders]
    pkgs_to_add <- append(pkgs_to_add,User.Views$Package[User.Views$Topic %in% binders])
    
    ## Error handling
    if(length(pkgs_to_add) == 0){
      stop(paste0("Binder ",binders[i]," not found."))
    }
    
    pkgs <- append(pkgs,pkgs_to_add)
  }
  rm(Topics.Views)
  rm(User.Views)
  
  return(unique(pkgs))
  
}


#' List Binders
#'
#' Function to see the binders of packages available and a summary for each. 
#' @usage list_binders()
#' @param compartment The compartment to view the binders in. Defaults to "all". 
#' - "all" to view all the binders
#' - "master" to view binders that came with the package
#' - "user" to view binders created by the user
#' @examples list_binders(compartment="master")
#' 
#' list_binders()

list_binders <- function(compartment="all"){
  load("./R/sysdata.rda")
  compartment <- tolower(compartment)
  if(!(compartment %in% c("all","master","user"))){
    print("Error: Please enter either all, master or user compartments to look in.")
  }else if (compartment == "all"){
    binders <- Topics.Views
    binders <- rbind(binders,User.Views)
  }else if(compartment == "master"){
    binders <- Topics.Views
  }else if(compartment == "user"){
    binders <- User.Views
  }
  
  ## To Do: Add number of packages column, concatenated head of packages
  
  rm(Topics.Views)
  rm(User.Views)
  return(unique(binders$Topic))
}

#' View Binder
#'
#' Function to see the packages within a particular binder
#' @usage list_binders()
#' @param compartment The compartment to view the binders in. Defaults to "all". 
#' - "all" to view all the binders
#' - "master" to view binders that came with the package
#' - "user" to view binders created by the user
#' @examples list_binders(compartment="master")
#' 
#' list_binders()

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
  install.packages(unique(pkgs),verbose=FALSE)  
}

#' Uninstall Binders
#'
#' Function to uninstall the packages in the binder(s). 
#' @usage uninstall_binders(binders)
#' @param binders The binder(s) to uninstall 
#' @examples uninstall_binders("MLM")
#' 
#' uninstall_binders()

uninstall_binders <- function(binders){
  pkgs <- .get_packages(binders)
  remove.packages(pkgs)
}


#' Load Binders
#'
#' Function to load the packages in a binder(s). 
#' @usage load_binders(binders)
#' @param binders The binder or a vector of binders whose packages are to be loaded into the environment.  
#' @examples load_binders("MLM")
#' 

load_binders <- function(binders){
  pkgs <- .get_packages(binders)
  
  for(i in c(1:length(pkgs))){
    tryCatch(
      {
        library(pkgs[i],character.only = TRUE)      
      },
      error=function(cond){
        install.packages(pkgs[i])
        library(pkgs[i],character.only = TRUE)      
      } 
    )
  }
}

