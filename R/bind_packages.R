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
#' @param suggest : default = TRUE. Boolean indicating whether want suggestions on existing packages
#' @examples bind_packages(Package = c("a","b","c"),binder = "alphabets", source = "CRAN")
#' 
#' bind_packages()

bind_packages = function(Package = NULL, binder = NULL, source = NULL, suggest = TRUE){
  
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
  
  if(toupper(source) == 'CRAN'){
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
  

  ## Look for suggested packages
  
  ## First, check if the package name is an exact match to something already existing
  binder_match = binder %in% unique(rbind(Topics.Views, User.Views)$Topic)
  if(all(binder_match) == TRUE){suggest == FALSE}
  
  ## If at least on of the binders is not an exact match, search through current views and suggest.
  
  if(suggest == TRUE){
    search_string <- gsub(" ","|",paste(binder,collapse=" "))
    print("The following binders already exist, would you like to add to one of  them instead?")
    existing_binders = view_binders('all', search = search_string)
    print(existing_binders)
    print("If yes, change the name of the binder to an existing one, otherwise turn the paramenter 'suggest' to FALSE")
  }
  
  else{
    
    
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
    
    
  }
  
  
  
  
  
  
  




## Remove Binders


