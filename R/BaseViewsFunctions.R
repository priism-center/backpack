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
  x <-as.character(unique(Topics.Views$Topic))
  ## To Do: Add number of packages column, concatenated head of packages
  rm(Topics.Views)
  return(x)
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
  load("./R/sysdata.rda")
  Topics.Views$Topic <- as.character(Topics.Views$Topic)
  Topics.Views$Package <- as.character(Topics.Views$Package)
  
  pkgs <- Topics.Views$Package[Topics.Views$Topic %in% binders]

  rm(Topics.Views)
  install.packages(pkgs,verbose=FALSE)
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
  load("./R/sysdata.rda")
  Topics.Views$Topic <- as.character(Topics.Views$Topic)
  Topics.Views$Package <- as.character(Topics.Views$Package)
  
  pkgs <- Topics.Views$Package[Topics.Views$Topic %in% binders]
  
  rm(Topics.Views)
  remove.packages(pkgs)
}
