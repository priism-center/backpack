## List of views- based on ctv available.views ## 

list.base.views <- function(){
  load("./R/BaseViews.rda")
  x <-as.character(unique(Topics.Views$Topic))
  rm(Topics.Views)
  return(x)
}

## Test ## 

list.base.views()

## Installing views - based on ctv install.views ##  -

install.base.views <- function(views){
  load("./R/BaseViews.rda")
  Topics.Views$Topic <- as.character(Topics.Views$Topic)
  Topics.Views$Package <- as.character(Topics.Views$Package)
  pkgs<-na.omit(ifelse(is.element(Topics.Views$Topic, views), Topics.Views$Package, NA))
  rm(Topics.Views)
  for(i in seq_along(pkgs)) install.packages(pkgs[[i]])
  invisible()
}
