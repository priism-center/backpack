## Add Binder function



## This function let's you add packages to an existing binder
## or creates a new binder a new name is provided

bind_packages = function(Package = NULL, binder = NULL, source = NULL){
  
  if(!file.exists("user_data.rda")){
    load("./R/sysdata.rda")
    user_data = Topics.Views
    save(user_data, file = "./R/user_data.rda")
    rm(Topics.Views)
    
    #print("user_data.rda file was create. this will contain all your binders!")
  }
  else{
    load(".R/user_data.rda")
  }
  
  
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
  user_data =  rbind(user_data, to_bind)
  user_data = user_data[order(user_data$Topic, user_data$Package),]
  save(user_data, file = "R/user_data.rda")
  rm(user_data)
}
