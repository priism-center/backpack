## Add Binder function


## This function let's you add packages to an existing binder
## or creates a new binder a new name is provided


#' Bind Packages
#'
#' Function to see the binders of packages available and a summary for each. 
#' @usage bind_packages(Package, binder, source)
#' @param Package : new packages to add
#' @param binder : either an existing binder or a new user-defined binder
#' @param source : whether package source is CRAN or Github
#' @examples bind_packages(Package = c("a","b","c"),binder = "alphabets", source = "CRAN")
#' 
#' bind_packages()

bind_packages = function(Package = NULL, binder = NULL, source = NULL){
  
  load("./R/sysdata.rda")
  
  ### To Do: check if packages are legit ?
  
  if(length(binder) == length(Package) | length(binder) == 1){
    Topic = binder
  }
  
  else{
    print("Either provide a single binder for all your packages, or one binder for each!")
    stop()
  }
  

  if(length(source) == length(Package) | length(binder) == 1){
    Source = source
  }
  else{
    print("Either provide a single source for all your packages, or one source for each!")
    stop()
  }
  
  
  to_bind = data.frame(Package, Topic, Source)
  User.Views <- rbind(User.Views,to_bind)
  User.Views$Package <- as.character(User.Views$Package)
  User.Views$Topic <- as.character(User.Views$Topic)
  User.Views$Source <- as.character(User.Views$Source)
  
  ### Account for duplicate package-Topic combinations
  User.Views = User.Views[!duplicated(User.Views),]
  
  
  save(User.Views,Topics.Views, file = "./R/sysdata.rda")
  rm(User.Views,Topics.Views)
}




## Remove Binders





