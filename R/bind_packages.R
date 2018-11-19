## Add Binder function


## This function let's you add packages to an existing binder
## or creates a new binder a new name is provided


#' Bind Packages
#'
#' Function to add packages to existing binders to the user views, or to create
#' new binders to store packages
#' @usage bind_packages(Package, binder, source)
#' @param Package : new packages to add
#' @param binder : either an existing binder or a new user-defined binder
#' @param source : whether package source is CRAN or Github Repo. If the source is a GitHub repo, please provide 
#' the `username/repo[/subdir]`
#' @examples bind_packages(Package = c("a","b","c"),binder = "alphabets", source = "CRAN")
#' 
#' bind_packages()

bind_packages = function(Package = NULL, binder = NULL, source = NULL){
  
  if(is.null(Package)){
    stop("argument 'Package' is missing with no default")
  }
  
  if(is.null(binder)){
    stop("argument 'binder' is missing with no default")
  }
  if(is.null(source)){
    stop("argument 'source' is missing with no default")
  }
  
  ## Check if package exists in CRAN
  
  if(toupper(Source) == 'CRAN'){
    cran_available = available.packages()[,1]
    is_available = Package %in% cran_available
    
    if(!all(is_available == rep(TRUE, length(is_available)))){
      to_print = paste('The package(s): ', toString(Package[is_available == FALSE]), 
                       ' are not available on CRAN!', collapse = '')
      stop(to_print)
    }
  }
  
  
  
  load("./R/sysdata.rda")
  
  
  
  
  if(length(binder) == length(Package) | length(binder) == 1){
    Topic = binder
  }
  
  else{
    stop("Either provide a single binder for all your packages, or one binder for each!")
  }
  

  if(length(source) == length(Package) | length(binder) == 1){
    Source = source
  }
  else{
    stop("Either provide a single source for all your packages, or one source for each!")
  }
  

  ## To do: Account for lower-upper case inside TOPIC (binder)
  existing_binders = list_binders('all')
  if(Topic %in% existing_binders)
  
  
  
  
  
  
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


# search_string <- gsub(" ","|",paste(binders,collapse=" "))


