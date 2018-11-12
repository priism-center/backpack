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
  
  pkgs <- Topics.Views$Package[Topics.Views$Topic %in% binders]

  rm(User.Views,Topics.Views)
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
  
  pkgs <- Topics.Views$Package[Topics.Views$Topic %in% binders]
  
  rm(User.Views,Topics.Views)
  remove.packages(pkgs)
}
