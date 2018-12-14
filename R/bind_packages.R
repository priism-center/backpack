## Add Binder function


## This function let's you add packages to an existing binder
## or creates a new binder a new name is provided


#' Bind Packages
#'
#' Function to add packages to existing binders to the user views, or to create
#' new binders to store packages
#' @usage bind_packages(Package, binder, source, suggest)
#' @param package : new packages to add
#' @param binder : either an existing binder or a new user-defined binder
#' @param source : whether package source is CRAN or Github Repo. If the source is a GitHub repo, please provide 
#' the `username/repo[/subdir]`
#' @param suggest : default = TRUE. Boolean indicating whether want suggestions on existing packages. Once you chose the final name, set
#' suggest = FALSE to either add packages to an existing binder or create a new binder. 
#' @examples bind_packages(Package = c("a","b","c"),binder = "alphabets", source = "CRAN", suggest = FALSE)
#' 
#' 
#'




## Remove Binders


## This function let's you remove packages from user-defined binders
## or creates remove entire binders. 


#' Unbind Packages
#'
#' Function to remove packages from existing binders, or entire binders, from user views. 
#' @usage unbind_packages(Package, binder)
#' @param package : package(s) to remove. Pass a vector if multiple packages.
#' @param binder : specifies the binder from which Packages need to be removed. If Packages is NULL, will remove entire binder. 
#' @examples unbind_packages(Package = c("a","b"), binder = "alphabets") 
#' unbind_packages(binder = 'alphabets')
#' 
#' 
#'

unbind_packages = function(package = NULL, binder = NULL){
  
  if(is.null(binder)){
    stop("argument 'binder' is missing with no default")
  }

  load("./R/sysdata.rda")
  
  Package = package
  
  if(length(binder) == length(package)){
    Binder = binder
  } else if(length(binder) == 1){
    Binder = ifelse(is.null(package), binder, rep(binder,length(package)))
  } else{
    stop("Either provide a single binder for all your packages, or one binder for each!")
  }
  

    User.Views$Package <- as.character(User.Views$Package)
    User.Views$Binder <- as.character(User.Views$Binder)
    User.Views$Topic <- as.character(User.Views$Topic)
    User.Views$Source <- as.character(User.Views$Source)
    
    if(is.null(package)){
      User.Views = User.Views[!(User.Views$Binder %in% Binder) ,]
    }
    
    else{
      
      User.Views = User.Views[!(User.Views$Binder %in% Binder & User.Views$Package %in% Package), ]
      
    }
    
    
    User.Views = User.Views[!duplicated(User.Views),]
    
    save(User.Views,Topics.Views, file = "./R/sysdata.rda")
    rm(User.Views,Topics.Views)
  }






