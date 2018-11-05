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
  load("./R/BaseViews.rda")
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
#' - "all" to view all the binders
#' - "master" to view binders that came with the package
#' - "user" to view binders created by the user
#' @examples install_binders("MLM")
#' 
#' install_binders()


install_binders <- function(binders){
  load("./R/BaseViews.rda")
  Topics.Views$Topic <- as.character(Topics.Views$Topic)
  Topics.Views$Package <- as.character(Topics.Views$Package)
  
  # pkgs<-na.omit(ifelse(is.element(Topics.Views$Topic, binders[i]), Topics.Views$Package, NA))  
  
  pkgs <- Topics.Views$Package[Topics.Views$Topic %in% binders]
  pkgs <- paste(pkgs,collapse = ",")
  rm(Topics.Views)
  install.packages(pkgs)
  invisible()
}
