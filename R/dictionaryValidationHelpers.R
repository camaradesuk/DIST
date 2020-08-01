#Helper functions for extract disease and drugs 

require(tidyr)
require(AutoAnnotation)

#---- Annotation Extraction helpers ----
ExtractAnnotation  <- function(myStudies , dictionaryName, ignoreCase = T, textSearchingHeaders = c( "CleanTitle" ,"CleanAbstract"), idColumn ="idStr"){
  results <- CountTermsInStudies(searchingData = myStudies
                                 , dictionary = dictionaryName
                                 , dictionaryNameHeader = "Id"
                                 , dictionaryRegexHeader = "Regex"
                                 , textSearchingHeaders = textSearchingHeaders
                                 , ignoreCase = ignoreCase
  )
  
  resultsWide <- data.frame(sapply(results[, -1], function(x) as.numeric(as.character(x))))
  resultsWide[resultsWide==0] <- NA
  resultsWide[,"Id"] <- myStudies[,idColumn]

  resultsLong <- resultsWide %>%
    gather(key = "Var", value="Frequency", -"Id", na.rm = T) %>%
    mutate(Var = as.numeric(gsub("X","",Var))) %>%
    left_join(GetData(dictionaryName), by = c("Var"="Id"))
  
  names(resultsLong)[names(resultsLong) =="Id"] <- idColumn
  
  return(resultsLong)
}

TagFrequentAnnotation  <- function(resultsLong, idColumn ="idStr", minimumIncludeFrequency = 9, maximumExcludeFrequency = 1){
  names(resultsLong)[names(resultsLong) == idColumn] <- "Id"
  
  resultsLongFiltered <- resultsLong %>%
    group_by(Id) %>%
    mutate(
      Count = n(),
      MaxFrequency = max(Frequency),
      Max = Frequency == MaxFrequency,
      Vars = paste(Var, collapse = ","),
      VarFrequency = paste(Frequency, collapse = ","),
      MaxVar = paste(Var[which(Frequency == MaxFrequency)], collapse = ","),
      MaxVarFrequency = paste(Frequency[which(Frequency == MaxFrequency)], collapse = ","),
      Selected = (Frequency >= minimumIncludeFrequency) | ((MaxFrequency - Frequency) < maximumExcludeFrequency)
    )
  
  names(resultsLongFiltered)[names(resultsLongFiltered) == "Id"] <- idColumn
  return(resultsLongFiltered)
}

GroupSelectedAnnotation <- function(resultsLongFiltered, idColumn ="idStr", varnames = c("Terms","Frequencies"), groupAnnotation = F){
  names(resultsLongFiltered)[names(resultsLongFiltered) == idColumn] <- "Id"
  
  if(groupAnnotation) {
    groupedData <- as.data.frame(resultsLongFiltered) %>%
      filter(Selected) %>%
      group_by(Id) %>%
      summarise(
        Terms = paste(Name, collapse = "||"),
        Frequencies = paste(Frequency, collapse = "||")
      )
  }else {
    groupedData <- as.data.frame(resultsLongFiltered) %>%
      filter(Selected) %>%
      select(Id, Terms = Name, Frequencies = Frequency)
  }

  names(groupedData) <- c(idColumn,  varnames)
  
  return(groupedData) 
}

AnnotateStudiesWithDictionary <- function(myStudies, dictionaryName, textSearchingHeaders = c( "CleanTitle" ,"CleanAbstract") , ignoreCase = T, idColumn = "idStr", varnames = c("Terms",    "Frequencies"), minimumIncludeFrequency = 5, maximumExcludeFrequency = 1, groupAnnotation = F){
  resultsLong <- ExtractAnnotation(myStudies, dictionaryName, idColumn = idColumn, ignoreCase = ignoreCase, textSearchingHeaders = textSearchingHeaders)
  resultsLongFiltered <- TagFrequentAnnotation(resultsLong, idColumn = idColumn, minimumIncludeFrequency = minimumIncludeFrequency , maximumExcludeFrequency = maximumExcludeFrequency)
  
  groupedData <- GroupSelectedAnnotation(resultsLongFiltered, idColumn = idColumn, varnames = varnames)
  annotatedData <- myStudies[,idColumn ,drop=F] %>%
    full_join(groupedData)

  return(annotatedData)
}

ExtractDisease <- function(myStudies, dictionaryName, idColumn = "idStr", textSearchingHeaders = c("Title","Abstract"), minimumIncludeFrequency = 9,  maximumExcludeFrequency = 1,groupAnnotation=F){
  myDiseases <- AnnotateStudiesWithDictionary(myStudies, dictionaryName, textSearchingHeaders = textSearchingHeaders, ignoreCase = T, idColumn = idColumn, minimumIncludeFrequency = minimumIncludeFrequency, maximumExcludeFrequency = maximumExcludeFrequency, varnames = c("Disease", "DiseaseFrequency"),groupAnnotation=groupAnnotation)
  return(myDiseases)
}

ExtractDrug <- function(myStudies, dictionaryName, idColumn = "idStr", textSearchingHeaders = c("Title","Abstract"), minimumIncludeFrequency = 9,  maximumExcludeFrequency = 1,groupAnnotation = groupAnnotation){
  myDrugs <- AnnotateStudiesWithDictionary(myStudies, dictionaryName, textSearchingHeaders = textSearchingHeaders, ignoreCase = T, idColumn = idColumn
                                           , minimumIncludeFrequency = minimumIncludeFrequency, maximumExcludeFrequency = maximumExcludeFrequency
                                           , varnames = c("Drug", "DrugFrequency"),groupAnnotation=groupAnnotation)
  return(myDrugs)
}

ExtractDrugDisease <- function(myStudies, diseaseDictionary, drugDictionary, idColumn = "idStr", minimumIncludeFrequency = 9,  maximumExcludeFrequency = 1, groupAnnotation = groupAnnotation){  
  myDiseases <- ExtractDisease(myStudies, dictionaryName = diseaseDictionary, idColumn = idColumn, minimumIncludeFrequency = minimumIncludeFrequency,  maximumExcludeFrequency = maximumExcludeFrequency, groupAnnotation = groupAnnotation)
  
  myDrugs <- ExtractDrug(myStudies, dictionary = drugDictionary, idColumn = idColumn, minimumIncludeFrequency = minimumIncludeFrequency,  maximumExcludeFrequency = maximumExcludeFrequency, groupAnnotation = groupAnnotation)
  
  myStudiesDiseaseDrugAnnotated <- myStudies %>%
    full_join(myDiseases) %>%
    full_join(myDrugs)
  
  return(myStudiesDiseaseDrugAnnotated)
}
