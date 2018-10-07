##### Make Tables Function #####

MakeTables <- function(data){
  
    #make dependencies table
    Dependencies <- data %>%
    select(Package, Depends.) %>%
    mutate(Depends. = strsplit(as.character(Depends.), ",")) %>% 
    unnest(Depends.)

    names(Dependencies)[2] <- "Dependencies"
    Dependencies$Dependencies <- trimws(Dependencies$Dependencies)

    Dependencies <- Dependencies %>%
    filter(!is.na(Dependencies))

    Dependencies <- Dependencies %>%
    filter(!grepl(pattern = "^R", Dependencies))
    
    #make in views table 
    InViews <- data %>%
      select(Package, In.views.) %>%
      mutate(In.views. = strsplit(as.character(In.views.), ","))%>%
      unnest(In.views.)
    
    names(InViews)[2] <- "InViews"
    InViews$InViews <- trimws(InViews$InViews)
    
    InViews <- InViews %>%
      filter(!is.na(InViews))
    
    #make imports table 
    
    Imports <- data %>%
      select(Package, Imports.) %>%
      mutate(Imports. = strsplit(as.character(Imports.), ","))%>%
      unnest(Imports.)
    
    names(Imports)[2] <- "Imports"
    Imports$Imports <- trimws(Imports$Imports)
    
    Imports <- Imports %>%
      filter(!is.na(Imports))
    
    #make Linking To
    
    LinkingTo <- data %>%
      select(Package, LinkingTo.) %>%
      mutate(LinkingTo. = strsplit(as.character(LinkingTo.), ","))%>%
      unnest(LinkingTo.)
    
    names(LinkingTo)[2] <- "LinkingTo"
    LinkingTo$LinkingTo <- trimws(LinkingTo$LinkingTo)
    
    LinkingTo <- LinkingTo %>%
      filter(!is.na(LinkingTo))
    
    #make Suggests
    Suggests <- data %>%
      select(Package, Suggests.) %>%
      mutate(Suggests. = strsplit(as.character(Suggests.), ","))%>%
      unnest(Suggests.)
    
    names(Suggests)[2] <- "Suggests"
    Suggests$Suggests <- trimws(Suggests$Suggests)
    
    Suggests <- Suggests %>%
      filter(!is.na(Suggests))
    
    #make Enhances 
    
    Enhances <- data %>%
      select(Package, Enhances.) %>%
      mutate(Enhances. = strsplit(as.character(Enhances.), ","))%>%
      unnest(Enhances.)
    
    names(Enhances)[2] <- "Enhances"
   Enhances$Enhances <- trimws(Enhances$Enhances)
    
    Enhances <- Enhances %>%
      filter(!is.na(Enhances))
    
    #create reverse depends 
    
    ReverseDepends <- data %>%
      select(Package, Reverse.depends.) %>%
      mutate(Reverse.depends. = strsplit(as.character(Reverse.depends.), ","))%>%
      unnest(Reverse.depends.)
    
    names(ReverseDepends)[2] <- "ReverseDepends"
    ReverseDepends$ReverseDepends <- trimws(ReverseDepends$ReverseDepends)
    
    ReverseDepends <-ReverseDepends %>%
      filter(!is.na(ReverseDepends))
    
    #create reverse imports
    
    ReverseImports <- data %>%
      select(Package, Reverse.imports.) %>%
      mutate(Reverse.imports. = strsplit(as.character(Reverse.imports.), ","))%>%
      unnest(Reverse.imports.)
    
    names(ReverseImports)[2] <- "ReverseImports"
    ReverseImports$ReverseImports <- trimws(ReverseImports$ReverseImports)
    
   ReverseImports <- ReverseImports %>%
      filter(!is.na(ReverseImports))
   
   #create reverse suggests 
   
   ReverseSuggests <- data%>%
     select(Package, Reverse.suggests.) %>%
     mutate(Reverse.suggests. = strsplit(as.character(Reverse.suggests.), ","))%>%
     unnest(Reverse.suggests.)
   
   names(ReverseSuggests)[2] <- "ReverseSuggests"
   ReverseSuggests$ReverseSuggests <- trimws(ReverseSuggests$ReverseSuggests)
   
   ReverseSuggests <- ReverseSuggests %>%
     filter(!is.na(ReverseSuggests))
   
   #create reverse linking to
   
    ReverseLinkingTo <- data%>%
     select(Package, Reverse.linking.to.) %>%
     mutate(Reverse.linking.to. = strsplit(as.character(Reverse.linking.to.), ","))%>%
     unnest(Reverse.linking.to.)
   
   names(ReverseLinkingTo)[2] <- "ReverseLinkingTo"
   ReverseLinkingTo$ReverseLinkingTo <- trimws(ReverseLinkingTo$ReverseLinkingTo)
   
   ReverseLinkingTo <- ReverseLinkingTo%>%
     filter(!is.na(ReverseLinkingTo))
   
   #create reverse enhances 
   
   ReverseEnhances <- data %>%
     select(Package, Reverse.enhances.) %>%
     mutate(Reverse.enhances. = strsplit(as.character(Reverse.enhances.), ","))%>%
     unnest(Reverse.enhances.)
   
   names(ReverseEnhances)[2] <- "ReverseEnhances"
   ReverseEnhances$ReverseEnhances <- trimws(ReverseEnhances$ReverseEnhances)
   
  ReverseEnhances <- ReverseEnhances%>%
     filter(!is.na(ReverseEnhances))
    
return(list(Dependencies, InViews, Imports, LinkingTo, Suggests, Enhances, ReverseDepends, ReverseImports,
            ReverseSuggests, ReverseLinkingTo, ReverseEnhances))
}

save(MakeTables,file = "MakeTables.rda")
