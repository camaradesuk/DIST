require(dplyr)
require(tidyr)
require(XML)    
require('seplyr')
source("R/syrfConfigure.R")

# Extract Annotatinos -----------------------------------------------------
grouped_first <- function(data, grouping_variables, value_variables) {
  result_names <- paste0(value_variables)
  expressions <- paste0("first(", value_variables, ")")
  data %>%
    group_by_se(grouping_variables) %>%
    summarize_se(c(result_names := expressions,
                   "count" := "n()"))
}

paste2<- function(strings, collapse = "", sep="")
{
  index <- which(!is.na(strings))
  if(length(index) >0 )  return(paste0(strings[index], collapse = collapse, sep=sep))
  else return("")
}

ExtractAnnotationAndChildrenForIdstr <- function(annotations, idStr){
  thisAnnotation <- annotations[which(annotations$idStr == idStr),] 
  outputAnnotations <- thisAnnotation
  
  if(nrow(thisAnnotation)==0)
  {
    # errorLog
    # print(paste0("Error found: ", idStr))
    return(outputAnnotations)
  } 
    if(thisAnnotation$HaveChildren) 
    {
      for(iiChildrenIdstr in  thisAnnotation$ChildrenIdstrs[[1]])
      {
        outputAnnotations <- rbind(outputAnnotations,
                                   ExtractAnnotationAndChildrenForIdstr(annotations, iiChildrenIdstr))
      }
    }
  
  return(outputAnnotations)
}

RemoveNewLine <- function(text){
  text <- gsub("/(\r\n)+|\r+|\n+|\t+/i", " ", text)
}

CleanAnnotations <- function(annotations){ 
  systemOtherQuestions <- system_other_questions()
  annotations$HaveChildren <- sapply(annotations$ChildrenIdstrs, function(x) length(x) >0 )
  annotations$Answer <- sapply(annotations$Answer, RemoveNewLine)
  annotations$Notes <- sapply(annotations$Notes, RemoveNewLine)
  
  annotations$Question[annotations$QuestionIdStr == "ecb550a5-ed95-473f-84bf-262c9faa7541"] <- systemOtherQuestions$diseaseModelLink
  annotations$Question[annotations$QuestionIdStr == "a3f2e5bb-3ade-4830-bb66-b5550a3cc85b"] <- systemOtherQuestions$interventionLink
  
  annotations$Question[annotations$QuestionIdStr == "b18aa936-a4c6-446b-ac98-88ac38930878" ]<- systemOtherQuestions$modelControl
  annotations$Question[annotations$QuestionIdStr ==  "d04ec2d7-3e10-4847-9999-befe7ee4c454" ]<- systemOtherQuestions$interventionControl
  
  # index <- which(annotations$Notes == "")
  # if(length(index) > 0) annotations$Notes[index] <- NULL
  return(annotations)
}


ValidateAnnotations <- function(extractedAnnotations){
  firstName <- names(extractedAnnotations)[1]
  if(is.null(firstName)) return(NULL)
  
  if(firstName == "idStr") {
    groupbyStr <- c("QuestionIdStr", "StudyIdStr")
  }else { 
    groupbyStr <- c(firstName, "QuestionIdStr", "StudyIdStr")
  }
  
  validExtractedAnnotations <- extractedAnnotations %>%
    arrange(desc(DateTimeCreated)) %>%
    group_by_at(groupbyStr) %>%
    mutate(latestInvestigatorIdStr = first(InvestigatorIdStr)) %>%
    filter(InvestigatorIdStr != "00000000-0000-0000-0000-000000000000" 
           | InvestigatorIdStr != latestInvestigatorIdStr) 
  
  validExtractedAnnotations[, "latestInvestigatorIdStr"] <- NULL
  
  return(validExtractedAnnotations)
}

ExtractRootAnnotations <- function(annotations){
  topAnnotations <- annotations %>%
    filter(Root == TRUE)
  topColumnNames <- c("StudyIdStr", "AuthorYear")
  
  extractedAnnotations <- cbind(data.frame(topIdstr = character(), topAnswer = character())
                                , as.data.frame(annotations[0,]))
  # if(nrow(topAnnotations) == 0) errorLog 
  for(irow in seq_along(topAnnotations$idStr)){
    topAnnotation <- topAnnotations[irow,]
    first2Columns <- data.frame(idStr = topAnnotation$StudyIdStr, paste0(topAnnotation$Author,topAnnotation$Year))
    extractedAnnotations <- 
      rbind(extractedAnnotations
            , cbind(first2Columns
                    , ExtractAnnotationAndChildrenForIdstr(annotations, topAnnotation$idStr)))
  }
  names(extractedAnnotations)[1:2] <- topColumnNames
  extractedAnnotations <- ValidateAnnotations(extractedAnnotations)
  return(extractedAnnotations)
}

ExtractAnnotationsForTopQuestion <- function(annotations, topQuestion = "", topColumnNames = c("topIdstr", "topAnswer")){
  systemRootQuestions <- system_root_questions()
  if(topQuestion == ""){
    topAnnotations <- as.data.frame(annotations %>% filter(Root == TRUE))
    topAnnotations <- cbind(data.frame(topIdstr =  topAnnotations$StudyIdStr, 
                                       topAnswer = paste0(topAnnotations$Author,topAnnotations$Year)),
                            topAnnotations)
    
    index <- which(topAnnotations$Question %in% systemRootQuestions)
    if(length(index) >0)    topAnnotations <- topAnnotations[-index, ]

  }
  else {
    topAnnotations <- FilterAnnotation(annotations, topQuestion)
    topAnnotations <- cbind(data.frame(topIdstr =  topAnnotations$idStr, 
                                       topAnswer = topAnnotations$Answer),
                            topAnnotations)
  }
  extractedAnnotations <- topAnnotations[0,]
  # if(nrow(topAnnotations) == 0) errorLog 
  for(irow in seq_along(topAnnotations$idStr)){
    # print(irow)
    topAnnotation <- topAnnotations[irow,]
    extractedAnnotations <- 
      rbind(extractedAnnotations
            , cbind(topAnnotation[c("topIdstr", "topAnswer")]
                    , ExtractAnnotationAndChildrenForIdstr(annotations, topAnnotation$idStr), row.names = NULL))
  }
  names(extractedAnnotations)[1:2] <- topColumnNames
  extractedAnnotations <- ValidateAnnotations(extractedAnnotations)
  return(extractedAnnotations)
}

SpreadAnnotations <- function(myAnnotations){
  index <- grep("DateTimeCreated", names(myAnnotations))
  myAnnotations <- myAnnotations[, -index]
  
  names <- names(myAnnotations)[1:2]
  if(nrow(myAnnotations) == 0) return(myAnnotations[,c( names,  "idStr","QuestionIdStr"  ,   "Question", "Root","Notes" , "Answer",
                                                        "StudyIdStr" , "Title","Abstract","Author","Year", "Investigator", "DOI","Journal", "InvestigatorIdStr")])
  
  firstName <- names(myAnnotations)[1]
  if(is.null(firstName)) return(NULL)
  if(firstName == "idStr") {
    groupbyStr <- c("QuestionIdStr", "StudyIdStr")
  }else { 
    groupbyStr <- c(firstName, "QuestionIdStr", "StudyIdStr")
  }
  
  topIdstrName <- names(myAnnotations)[1]
  groupbyColumns <- c(topIdstrName, "StudyIdStr", "InvestigatorIdStr", "QuestionUnique")
  a <- myAnnotations %>%
    mutate(
      QuestionUnique = paste(QuestionIdStr, Question, sep="_"),
      QuestionUniqueId =paste(QuestionUnique, "Id", sep="_"),
      QuestionUniqueNotes = paste(QuestionUnique, "Notes", sep="_"),
      Answer = paste(unlist(Answer),  collapse = ";")
    ) %>%
    group_by_at(groupbyColumns) %>%
    summarise( 
      GroupedAnswers = paste2(Answer, collapse = ";"),
      GroupedNotes = paste2(Notes, collapse = ";"),
      GroupedIdStrs = paste2(idStr, collapse = ";"),
      Title = first(Title),
      Author = first(Author),
      Abstract = first(Abstract),
      Year = first(Year),
      DOI = first(DOI),
      Journal = first(Journal),
      Investigator = first(Investigator),
      QuestionUniqueId = first(QuestionUniqueId),
      QuestionUniqueNotes = first(QuestionUniqueNotes)
    )
  b <- a %>%
    select(topIdstrName, "StudyIdStr","InvestigatorIdStr", "QuestionUnique"  ,"GroupedAnswers", "Title","Author","Abstract", "Year", "Journal", "DOI", "Investigator") %>%
    spread(QuestionUnique, GroupedAnswers ) 
  
  c <- a %>%
    select(topIdstrName, "StudyIdStr","InvestigatorIdStr" , "QuestionUniqueId"  ,"GroupedIdStrs" ) %>%
    spread(QuestionUniqueId, GroupedIdStrs )
  
  d <- a %>%
    select(topIdstrName, "StudyIdStr",    "InvestigatorIdStr",  "QuestionUniqueNotes"  ,    "GroupedNotes"  ) %>%
    spread(QuestionUniqueNotes, GroupedNotes )
  
  results <- b %>%
    left_join(c,by = c(topIdstrName, "StudyIdStr", "InvestigatorIdStr")) %>%
    left_join(d,by = c(topIdstrName, "StudyIdStr", "InvestigatorIdStr"))
  
  index <- order(names(results)[10:ncol(results)])+9

  results <- results[, c(1:9,index)]
  return(results)
}

# Extract Outcomes --------------------------------------------------------
CleanOutcomes <- function(outcomes){
  index <- grep("id$",names(outcomes),ignore.case = T)
  if(length(index) >0)  outcomes[,index] <- NULL
  
  index <- grep("Reconciled",names(outcomes),ignore.case = T)
  if(length(index) >0)  outcomes[,index] <- NULL
  return(outcomes)
}

ValidateOutcome <- function(outcomes){
  
  if(!is.null(outcomes))
  {
    validOutcomes <- outcomes %>%
      arrange(desc(DateTimeCreated)) %>%
      group_by(StudyIdStr, ExperimentIdStr, CohortIdStr, OutcomeIdStr, TimeInMinute) %>%
      mutate(latestInvestigatorIdStr = first(InvestigatorIdStr)) %>%
      filter(InvestigatorIdStr != "00000000-0000-0000-0000-000000000000" 
             | InvestigatorIdStr != latestInvestigatorIdStr) 
    validOutcomes$latestInvestigatorIdStr <- NULL
    
    # errorIdstr <- outcomes %>% 
    #   anti_join(validOutcomes, by=c("idStr","idStr"))
    
    # Errorlog
  }
  else
  {
    
    return(NULL)
  }
  
  return(validOutcomes)
}


combineOutcomeData <- function(outcomes, rootAnnotationsSpread, cohortAnnotationsSpread, outcomeAnnotationsSpread
                               , modelAnnotationsSpread,interventionAnnotationsSpread,  systemOtherQuestions)
{
  outcomeData <- as.data.frame(outcomes) %>%
    left_join(rootAnnotationsSpread)%>%
    # left_join(experimentAnnotationsSpread)%>%
    left_join(cohortAnnotationsSpread)  %>%
    left_join(outcomeAnnotationsSpread)
  
  index <- grep(paste0(systemOtherQuestions$diseaseModelLink,"$"), names(outcomeData))
  if(length(index)==1){
    outcomeData$ModelIdStr <- outcomeData[,index]
    outcomeData <- outcomeData %>%
      left_join(modelAnnotationsSpread)
  } else {
    outcomeData$ModelIdStr <- ""
    outcomeData$`Control procedure?`<- ""
  }
  
  index <- grep(paste0(systemOtherQuestions$interventionLink,"$"), names(outcomeData))
  if(length(index)==1)  {
    outcomeData$InterventionIdStr <- outcomeData[,index]
    outcomeData <- outcomeData %>%
      left_join(interventionAnnotationsSpread)
  }else {
    outcomeData$InterventionIdStr <- ""
    outcomeData$`Control procecdure?`<- ""
  }
  
  index <- grep("_Id$", names(outcomeData))
  outcomeData <- outcomeData[, -index]
  
  index <- which(nchar(names(outcomeData)) > 37) 
  names(outcomeData)[index] <- substr(names(outcomeData)[index], 38,1000)
  
  # They are the same qustions but are designed seperatly under different answers
  # We need think about the solusion for this 
  index <- which(duplicated(names(outcomeData)))
  if(length(index) > 0)
  {
    duplicatedNames <- unique(names(outcomeData)[index])
    for(duplicatedName in duplicatedNames)
    {
      index <- which(names(outcomeData) == duplicatedName)
      
      temp <- apply(outcomeData[, index], 1, paste2, collapse = ", ")
      
      outcomeData[, index[1]] <- temp
      
      outcomeData[, index[-1]] <- NULL
    }
  }
  
  return(outcomeData)
}

# Writing -----------------------------------------------------------------

unlistAnnotations <- function(mydf)
{
  while("list" %in% sapply(mydf, class))
  {
    for(name in names(mydf)) {
      if(class(mydf[,name])[1] == 'list') mydf[,name]  <- sapply(mydf[,name], paste, collapse = "," )
    }
  }
  return(mydf)
}

WriteTSV <- function(myData, filename = "output.txt"){
  if(!grepl("txt$", filename)) filename <- paste0(filename, ".txt")
  write.table(myData, file = filename, sep = '\t', quote = T, row.names = F, na = "")
  print(paste0("write output: ", filename))
  
}

WriteCSV <- function(myData, filename = "output.csv"){
  if(!grepl("csv$", filename)) filename <- paste0(filename, ".csv")
  write.csv(myData, file = filename, quote = T, row.names = F, na = "")
 print(paste0("write output: ", filename))
  }

WriteOutput <- function(outputAnnotations, myOutputFolder = "output/", filename = "output",
                        outputType = "csv"){
  while("list" %in% sapply(outputAnnotations, class))
  {
    for(name in names(outputAnnotations)) {
      if(class(outputAnnotations[,name])[1] == 'list') outputAnnotations[,name]  <- sapply(outputAnnotations[,name], paste, collapse = "," )
    }
  }
  filename <- paste0(myOutputFolder, filename)
  
  if(outputType == "csv")  WriteCSV(outputAnnotations, filename)
  if(outputType == "tsv")  WriteTSV(outputAnnotations, filename)
}

WriteAnnotations <- function(outputAnnotations, myOutputFolder, prefix = ""){
  filename <- paste0(prefix,'Annotation')
  WriteOutput(outputAnnotations, myOutputFolder = myOutputFolder, filename = filename,
  outputType = "csv")
}

WriteOutcomes <- function(outcomeData, myOutputFolder, prefix = ""){
  filename <- paste0(prefix, 'Outcomes')
  WriteOutput(outcomeData, myOutputFolder = myOutputFolder, filename = filename,
              outputType = "csv")
  }

# WriteAnnotationForStudy <- function(myOutputFolder, outputAnnotation, prefix = ""){
#   uniqueStudyIdStrs <-unique(outputAnnotation$StudyIdStr) 
#   
#   for(uniqueStudyIdStr in uniqueStudyIdStrs){
#     studyAnnotation <- outputAnnotation %>%  filter(StudyIdStr == uniqueStudyIdStr)
#     
#     uniqueCohortIdstrs <- unique(studyAnnotation$CohortIdStr)
#     
#     outputCohorts <- studyAnnotation[0,]
#     # output annotations for each cohort
#     for(i  in seq_along(uniqueCohortIdstrs))
#     {
#       uniqueCohortIdstr <- uniqueCohortIdstrs[i]
#       cohortAnnotation <- studyAnnotation %>% filter(CohortIdStr == uniqueCohortIdstr) %>% arrange(Question)
#       
#       cohortAnnotation <- cohortAnnotation %>% group_by(QuestionId) %>%
#         mutate(
#           # CohortLabels =  paste0(Cohort, collapse = ","),
#           Answers = paste(Answer, collapse = ","),
#           Notess =  paste(Notes, collapse = ",")
#         )  %>%
#         summarise(
#           Question = first(Question),
#           Cohort = first(Cohort),
#           Answers = first(Answers),
#           Notess = first(Notess)
#         )
#       
#       if(dim(cohortAnnotation)[1] != 34)  print(dim(cohortAnnotation))
#       
#       cohortAnnotation$Answers[which(cohortAnnotation$Answers == "999999")] <- "Not reported"
#       
#       cohortAnnotation$Answers <- trimws(cohortAnnotation$Answers)
#       
#       index <- which(names(cohortAnnotation) == "QuestionId")
#       names(cohortAnnotation)[-index] <- paste(names(cohortAnnotation)[-index], i, sep='.')
#       
#       if(nrow(outputCohorts) == 0)  { outputCohorts <- rbind(outputCohorts, cohortAnnotation) }
#       else { outputCohorts <- outputCohorts %>% left_join(cohortAnnotation, by = c("QuestionId" = "QuestionId")) }
#       # print(names(outputCohorts))
#     }
#     
#     if("Answers.1" %in% names(outputCohorts) & "Answers.2" %in% names(outputCohorts) & !("Answers.3" %in% names(outputCohorts))) outputCohorts <- outputCohorts %>% filter(Answers.1 != Answers.2)
#     
#     index <- grep('QuestionId', names(outputCohorts))
#     outputCohorts[,index] <- NULL
#     index <- grep('Question', names(outputCohorts))
#     outputCohorts[,index[-1]] <- NULL
#     
#     filename <- paste0(myOutputFolder, '/', prefix, paste0(studyAnnotation$Author[1], studyAnnotation$Year[1]), '_', cohortAnnotation$Cohort[1],'.txt')
#     write.table(outputCohorts, file = filename, sep = '\t', quote = FALSE, row.names = FALSE)
#     filename <- paste0(myOutputFolder, '/', prefix, paste0(studyAnnotation$Author[1], studyAnnotation$Year[1]),'.csv')
#     write.csv(outputCohorts , file = filename, quote = TRUE, row.names = FALSE)
#   }
# }
# 
# WriteAnnotationForEachCohort <- function(myOutputFolder, outputAnnotation, prefix = "ReviewerAnnotationData_"){
#   uniqueStudyIdStrs <-unique(outputAnnotation$StudyIdStr) 
#   for(uniqueStudyIdStr in uniqueStudyIdStrs)
#   {
#     studyAnnotation <- outputAnnotation %>%  filter(StudyIdStr == uniqueStudyIdStr)
#     
#     uniqueCohortIdstrs <- unique(studyAnnotation$CohortIdStr)
#     
#     # output annotations for each cohort
#     for(uniqueCohortIdstr in uniqueCohortIdstrs)
#     {
#       cohortAnnotation <- studyAnnotation %>% filter(CohortIdStr == uniqueCohortIdstr)
#       
#       filename <- paste0(myOutputFolder, '/', prefix, paste0(studyAnnotation$Author[1], studyAnnotation$Year[1]), '_', cohortAnnotation$Cohort[1],'.txt')
#       write.table(cohortAnnotation[, c("Cohort", "Question","Answer", "Notes")], file = filename, sep = '\t', quote = FALSE, row.names = FALSE)
#       filename <- paste0(myOutputFolder, '/', prefix, paste0(studyAnnotation$Author[1], studyAnnotation$Year[1]), '_', cohortAnnotation$Cohort[1],'.csv')
#       write.csv(cohortAnnotation[, c("Cohort", "Question","Answer", "Notes")] , file = filename, quote = TRUE, row.names = FALSE)
#     }
#   }
# }
# 
# WriteCohortLevelAnnotationForEachStudy <- function(myOutputFolder, outputAnnotation, prefix = "CohortLevel_"){
#   uniqueStudyIdStrs <-unique(outputAnnotation$StudyIdStr) 
#   
#   for(uniqueStudyIdStr in uniqueStudyIdStrs){
#     studyAnnotation <- outputAnnotation %>%  filter(StudyIdStr == uniqueStudyIdStr)
#     
#     uniqueCohortIdstrs <- unique(studyAnnotation$CohortIdStr)
#     
#     outputCohorts <- studyAnnotation[0,]
#     # output annotations for each cohort
#     for(i  in seq_along(uniqueCohortIdstrs))
#     {
#       uniqueCohortIdstr <- uniqueCohortIdstrs[i]
#       cohortAnnotation <- studyAnnotation %>% filter(CohortIdStr == uniqueCohortIdstr) %>% arrange(Question)
#       
#       cohortAnnotation <- cohortAnnotation %>% group_by(QuestionId) %>%
#         mutate(
#           # CohortLabels =  paste0(Cohort, collapse = ","),
#           Answers = paste2(Answer, collapse = ","),
#           Notess =  paste2(Notes, collapse = ",")
#         )  %>%
#         summarise(
#           Question = first(Question),
#           Cohort = first(Cohort),
#           Answers = first(Answers),
#           Notess = first(Notess)
#         )
#       
#       if(dim(cohortAnnotation)[1] != 34)  print(dim(cohortAnnotation))
#       
#       cohortAnnotation$Answers[which(cohortAnnotation$Answers == "999999")] <- "Not reported"
#       
#       cohortAnnotation$Answers <- trimws(cohortAnnotation$Answers)
#       
#       index <- which(names(cohortAnnotation) == "QuestionId")
#       names(cohortAnnotation)[-index] <- paste(names(cohortAnnotation)[-index], i, sep='.')
#       
#       if(nrow(outputCohorts) == 0)  { outputCohorts <- rbind(outputCohorts, cohortAnnotation) }
#       else { outputCohorts <- outputCohorts %>% left_join(cohortAnnotation, by = c("QuestionId" = "QuestionId")) }
#       # print(names(outputCohorts))
#     }
#     
#     if("Answers.1" %in% names(outputCohorts) & "Answers.2" %in% names(outputCohorts) & !("Answers.3" %in% names(outputCohorts))) outputCohorts <- outputCohorts %>% filter(Answers.1 != Answers.2)
#     
#     index <- grep('QuestionId', names(outputCohorts))
#     outputCohorts[,index] <- NULL
#     index <- grep('Question', names(outputCohorts))
#     outputCohorts[,index[-1]] <- NULL
#     
#     filename <- paste0(myOutputFolder, '/', prefix, paste0(studyAnnotation$Author[1], studyAnnotation$Year[1]), '_', cohortAnnotation$Cohort[1],'.txt')
#     write.table(outputCohorts, file = filename, sep = '\t', quote = FALSE, row.names = FALSE)
#     filename <- paste0(myOutputFolder, '/', prefix, paste0(studyAnnotation$Author[1], studyAnnotation$Year[1]),'.csv')
#     write.csv(outputCohorts , file = filename, quote = TRUE, row.names = FALSE)
#   }
# }

WriteAnnotationXML <- function(myOutputFolder, annotations, prefix = "Studies"){
  systemRootQuestions <- systemRootQuestions()
  top <- newXMLNode("Project", "Sudep")
  
  studyIdStrs <- unique(annotations$StudyIdStr)
  
  annotations$Answer <- gsub("[&]"," and ",annotations$Answer)
  annotations$Answer <- gsub("[>]"," larger than ",annotations$Answer)
  annotations$Answer <- gsub("<]"," less than ",annotations$Answer)
  annotations$Answer <- gsub("[\"]"," ",annotations$Question)
  annotations$Answer <- gsub("[\']"," ",annotations$Question)
  annotations$Answer <- gsub("[&]"," and ",annotations$Question)
  annotations$Answer <- gsub("[>]"," larger than ",annotations$Question)
  annotations$Answer <- gsub("<]"," less than ",annotations$Question)
  
  for(iStudy in seq_along(studyIdStrs))
  {
    studyIdstr <- studyIdStrs[iStudy]
    
    myAnnotations <- annotations %>% filter(StudyIdStr == studyIdstr)
    
    studyName <- paste0(myAnnotations$Author[1], myAnnotations$Year[1])
    
    myStudy <- newXMLNode("Study", studyName, parent=top)
    
    cohortLabels <- FilterAnnotation(myAnnotations, systemRootQuestions$cohortLabel)
    
    cohorts <- data.frame(Cohort = cohortLabels$Answer, CohortIdStr = cohortLabels$idStr)
    cohorts <- cbind(cohorts, cohortLabels)
    
    for(icohort in seq_along(cohorts$idStr))
    {
      cohort <- cohorts[icohort,]
      
      children0 <- newXMLNode("Annotation", cohort$Answer, attrs=c(Question = cohort$Question), parent=myStudy)
      # outputAnnotation <- rbind(outputAnnotation, cohort)
      
      if(!cohort$HaveChildren) next
      for(iiChildrenIdstr in cohort$ChildrenIdstrs[[1]])
      {
        iiannotation <- annotations[which(annotations$idStr == iiChildrenIdstr),]
        
        if(iiannotation$Question != "What are the results?") next
        
        iiannotation$CohortIdStr <- cohort$idStr
        iiannotation$Cohort <- cohort$Answer
        
        children1 <- newXMLNode("Annotation", iiannotation$Answer, attrs=c(Question = iiannotation$Question), parent=children0)
        # outputAnnotation <- rbind(outputAnnotation, iiannotation)
        
        if(!iiannotation$HaveChildren) next
        
        for(jjChildrenIdstr in iiannotation$ChildrenIdstrs[[1]])
        {
          jjannotation <- annotations[which(annotations$idStr == jjChildrenIdstr),]
          jjannotation$CohortIdStr <- cohort$idStr
          jjannotation$Cohort <- cohort$Answer
          
          children2 <- newXMLNode("Annotation", jjannotation$Answer, attrs=c(Question= jjannotation$Question), parent=children1)
          # outputAnnotation <- rbind(outputAnnotation, jjannotation)
          
          if(!jjannotation$HaveChildren) next
          for(kkChildrenIdstr in jjannotation$ChildrenIdstrs[[1]])
          {
            kkannotation <- annotations[which(annotations$idStr == kkChildrenIdstr),]
            kkannotation$CohortIdStr <- cohort$idStr
            kkannotation$Cohort <- cohort$Answer
            
            children3 <- newXMLNode("Annotation", kkannotation$Answer, attrs=c(Question= kkannotation$Question), parent=children2)
            # outputAnnotation <- rbind(outputAnnotation, kkannotation)
            
            if(!kkannotation$HaveChildren) next
            for(llChildrenIdstr in kkannotation$ChildrenIdstrs[[1]])
            {
              llannotation <- annotations[which(annotations$idStr == llChildrenIdstr),]
              llannotation$CohortIdStr <- cohort$idStr
              llannotation$Cohort <- cohort$Answer
              
              children4 <- newXMLNode("Annotation", llannotation$Answer, attrs=c(Question= llannotation$Question), parent=children3)
              # outputAnnotation <- rbind(outputAnnotation, llannotation)
              
              if(!llannotation$HaveChildren) next
              
              for(mmChildrenIdstr in llannotation$ChildrenIdstrs[[1]])
              {
                mmannotation <- annotations[which(annotations$idStr == mmChildrenIdstr),]
                mmannotation$CohortIdStr <- cohort$idStr
                mmannotation$Cohort <- cohort$Answer
                
                children5 <- newXMLNode("Annotation", mmannotation$Answer, attrs=c(Question= mmannotation$Question), parent=children4)
                # outputAnnotation <- rbind(outputAnnotation, mmannotation)
                
                if(mmannotation$HaveChildren) 
                { 
                  print("more children")
                }
                
                rm(mmannotation)
              }
              rm(llChildrenIdstr)
            }
            rm(kkannotation)
          } 
          rm(jjannotation)
        }
        rm(iiannotation)
      }
      rm(cohort)
    }
  }
  saveXML(top, file=paste0(myOutputFolder, '/', prefix,'.txt'))
}
