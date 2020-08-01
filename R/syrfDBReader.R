# Reader helpers for read SyRF mongo database
# The collection name, database name and connection string are not included for security reason
# Please do not spread this code as it may compromise SyRF database.
# made by Jing Liao in January 2018
# update by Jing Liao until Dec 2018

require(mongolite)
require(dplyr)
require(rjson)
options(stringsAsFactors = FALSE)

LoadCollections <-
  function(collectionName, dbname, connectionString)
  {
    con <- mongo(
      collection = collectionName
      ,
      db = dbname
      ,
      url =  connectionString ,
      verbose = FALSE,
      options = ssl_options()
    )
    
    return(con)
  }

GetProjects <- function(con)
{
  projects <- con$find(fields = '{"Name" : true, "ContactEmail": true, "_id":true}')
  projects$idStr <- sapply(projects$`_id`, ConvertRawIdToUUIDString)
  return(projects)
}

Count <- function(con)
{
  return(con$count())
}

GetProject <- 
  function(con, projectID, raw = FALSE)
  {
    if (raw == FALSE)
    {
      projectIDstr <- projectID
    } else {
      projectIDstr <-  openssl::base64_encode(projectID)
    }
    
    project <-
      con$find(
        query = paste0('{"_id": {"$binary": "', projectIDstr, '", "$type": "3"}}'), 
        fields = '{"_id" : true,
        "DateTimeCreated"      : true,           
        "Name" : true,
        "CreationDate"          : true,          
        "CompletionDate"               : true,   
        "CreatorId"                       : true,
        "OwnerId"                         : true,
        "SystematicSearchIds"             : true,
        "StudyIds"                        : true,
        "ProtocolLink"                    : true,
        "Keywords"                        : true,
        "InclusionCriteria"               : true,
        "ExclusionCriteria"               : true,
        "Notes"                           : true,
        "AgreementThreshold"              : true,
        "Stages"                          : true,
        "IsPublic"                        : true,
        "Archived"                        : true,
        "SecuritySettings"                : true,
        "MachineLearningScreeningSetting" : true,
        "LivingSearchSetting"             : true,
        "AnnotationQuestions"             : true,
        "Registrations"     : true
  }')
    
    project$idStr <- sapply(project$`_id`, ConvertRawIdToUUIDString)
    project$SystematicSearchIdStrs <- list(sapply(project$SystematicSearchIds[[1]]
                                            , function(x)  {ConvertRawIdToUUIDString(x)}))
    return(project)
    }

GetInvestigators <- function(con)
{
  investigators <-
    con$find(fields = '{"FullName" : true, "Email" : true,  "_id" : true}')
  
  return(investigators)
}

GetInvestigatorIdsForProject <-
  function(conProject, projectID, raw = FALSE)
  {
    if (raw == FALSE)
    {
      projectIDstr <- projectID
    } else {
      projectIDstr <-  openssl::base64_encode(projectID)
    }
    
    project <-
      conProject$find(query = paste0('{"_id": {"$binary": "', projectIDstr, '", "$type": "3"}}'))
    
    investigatorIds <-
      as.data.frame(project$Registrations)$InvestigatorId
    
    return(investigatorIds)
  }

GetInvestigatorsForProject <-
  function(conInvestigator,
           conProject,
           projectID,
           raw = FALSE)
  {
    investigatorIds <-
      GetInvestigatorIdsForProject(conProject, projectID, raw = raw)
    
    allInvestigators <- GetInvestigators(conInvestigator)
    
    index <- which(allInvestigators$`_id` %in% investigatorIds)
    
    investigators <- allInvestigators[index,]
    
    investigators$idStr <- sapply(investigators$`_id`, ConvertRawIdToUUIDString)
    investigators$Investigator <- investigators$FullName$Preferred
    
    investigators$Name <- paste(investigators$FullName$First, investigators$FullName$Surname)
    
    investigators$FullName <- NULL
    
    return(investigators)
  }

GetStudies <- function(con)
{
  studies <- con$find()
  return(studies)
}

CountStudyForProject <- function(con, projectID, raw = FALSE, minDateTime = "2016-04-01T00:00:00.000Z")
{
  if (raw == FALSE)
  {
    projectIDstr <- projectID
  } else {
    projectIDstr <-  openssl::base64_encode(projectID)
  }
  
  nStudies <- con$count(
    query = paste0(
      '{"$and": [{"ProjectId": {"$binary": "', projectIDstr, '", "$type": "3"}},{"DateTimeCreated": {"$gt": {"$date": "', minDateTime ,'"}}}]}'
    )
  )
  return(nStudies)
}

HasStudyForProject <- function(con, projectID, raw = FALSE, minDateTime = "2016-04-01T00:00:00.000Z")
{
  if (raw == FALSE)
  {
    projectIDstr <- projectID
  } else {
    projectIDstr <-  openssl::base64_encode(projectID)
  }
  
  study <- con$find(
    query = paste0(
      '{"$and": [{"ProjectId": {"$binary": "', projectIDstr, '", "$type": "3"}},{"DateTimeCreated": {"$gt": {"$date": "', minDateTime ,'"}}}]}'
    )
    ,
    fields = '{"_id" : true, "DateTimeCreated": true
    }'
    ,
    limit = 1
  )
  
  return(nrow(study) > 0)
}

GetStudiesForProject <- function(con, projectID, raw = FALSE, minDateTime = "2016-04-01T00:00:00.000Z")
{
  if (raw == FALSE)
  {
    projectIDstr <- projectID
  } else {
    projectIDstr <-  openssl::base64_encode(projectID)
  }

  studies <- con$find(
    query = paste0(
      '{"$and": [{"ProjectId": {"$binary": "', projectIDstr, '", "$type": "3"}},{"DateTimeCreated": {"$gt": {"$date": "', minDateTime ,'"}}}]}'
    )
    ,
    fields = '{"Title" : true,
    "ExtractionInfo": true,
    "ScreeningInfo" : true,
    "Year" : true,
    "PublicationName" : true,
    "Authors" : true,
    "AuthorAddress":true,
    "Year" : true,
    "Abstract" : true,
    "DOI": true,
    "_id" : true, 
    "Keywords": true,
    "ProjectId": true,
    "SystematicSearchId" : true,
"ReferenceType" : true,
"DateTimeCreated": true
    }'
  )
  
  studies$idStr <- sapply(studies$`_id`, ConvertRawIdToUUIDString)
  studies$ProjectIdStr <- sapply(studies$`ProjectId`, ConvertRawIdToUUIDString)
  studies$SystematicSearchIdStr <- sapply(studies$SystematicSearchId, ConvertRawIdToUUIDString)
  
  studies$Author <- sapply(studies$Authors, function(x) {
    if(!is.null(x$Order)){
      return(    paste0(x[which(x$Order == 0),]$FullName$Surname, collapse = ""))
    }
    return("")
  })
  studies$Author[] <- studies$Author
  
  studies$Authors <- sapply(studies$Authors, function(x) {paste0(paste0(x$FullName$First, ' ', x$FullName$Surname),collapse = ', ')})
  
  studies$Journal <- studies$PublicationName$Name
  studies$AltJournal <- studies$PublicationName$AlternateName
  
  studies$Abstract <- sapply(studies$Abstract,  function(x) {gsub("[\"]", "[\']",x)})
  
  return(studies)
  }

CountScreenings <- function(con)
{
  aggregatestr <-  paste0('[{
                          "$unwind": {
                          "path": "$ScreeningInfo.Screenings"
                          }}
                          ,{ "$count": "number"}]')
  
  return(con$aggregate(aggregatestr)$number)
  }

CountScreeningsForProject <- function(con, projectID, raw = FALSE)
{
  if (raw == FALSE)
  {
    projectIDstr <- projectID
  } else {
    projectIDstr <-  openssl::base64_encode(projectID)
  }
  
  aggregatestr <-  paste0(
    '[{
    "$match": {
    "ProjectId": {
    "$binary": "',
    projectIDstr,
    '",
    "$type": "3"
    }
    }
    }, {
    "$unwind": {
    "path": "$ScreeningInfo.Screenings"
    }
    },{ "$count": "number"}]'
)
  
  return(con$aggregate(aggregatestr)$number)
    }

GetScreenings <- function(con)
{
  aggregatestr <- paste0('[{
                         "$unwind": {
                         "path": "$ScreeningInfo.Screenings"
                         }
}]')

  studies <- con$aggregate(aggregatestr)
  
  screenings <- studies$ScreeningInfo$Screenings
  
  return(screenings)
  }

GetScreeningInfosForProject <- function(con, projectID, raw = FALSE)
{
  if (raw == FALSE)
  {
    projectIDstr <- projectID
  } else {
    projectIDstr <-  openssl::base64_encode(projectID)
  }
  
  aggregatestr <- paste0(
    '[{
    "$match": {
    "ProjectId": {
    "$binary": "',
    projectIDstr,
    '",
    "$type": "3"
    }
    }
    }, {
    "$unwind": {
    "path": "$ScreeningInfo"
    }
    }]'
  )
  
  studies <- con$aggregate(aggregatestr)
  
  screenings <- studies$ScreeningInfo
  
  return(screenings)
    }

GetScreeningsForProject <- function(con, projectID, raw = FALSE)
{
  if (raw == FALSE)
  {
    projectIDstr <- projectID
  } else {
    projectIDstr <-  openssl::base64_encode(projectID)
  }
  
  aggregatestr <- paste0(
    '[{
    "$match": {
    "ProjectId": {
    "$binary": "',
    projectIDstr,
    '",
    "$type": "3"
    }
    }
    }, {
    "$unwind": {
    "path": "$ScreeningInfo.Screenings"
    }
    }]'
  )
  
  studies <- con$aggregate(aggregatestr)
  
  screenings <- studies$ScreeningInfo$Screenings
  
  screenings$idStr <- sapply(screenings$`_id`, ConvertRawIdToUUIDString)
  screenings$ScreenerIdStr <- sapply(screenings$ScreenerId, ConvertRawIdToUUIDString)
  screenings$StudyIdStr <- sapply(screenings$StudyId, ConvertRawIdToUUIDString)
  
  return(screenings)
    }

GetAnnotations <- function(con)
{
  aggregatestr <-
    paste0('[{"$unwind": {"path": "$ExtractionInfo.Annotations"}}]')
  
  studies <- con$aggregate(aggregatestr)
  
  annotations <- studies$ExtractionInfo$Annotations
  
  annotations$idStr <-
    sapply(annotations$`_id`, ConvertRawIdToUUIDString)
  annotations$StudyIdStr <-
    sapply(annotations$StudyId, ConvertRawIdToUUIDString)
  annotations$InvestigatorIdStr <-
    sapply(annotations$AnnotatorId, ConvertRawIdToUUIDString)
  annotations$QuestionIdStr <-
    sapply(annotations$QuestionId, ConvertRawIdToUUIDString)
  
  annotations$ChildrenIdstrs <-
    sapply(annotations$Children, function(x)
      lapply(x, ConvertRawIdToUUIDString))
  
  return(annotations)
}

GetAnnotationSessionsForProject <-
  function(con, projectID, raw = FALSE)
  {
    if (raw == FALSE)
    {
      projectIDstr <- projectID
    } else {
      projectIDstr <-  openssl::base64_encode(projectID)
    }
    
    aggregatestr <-
      paste0(
        '[{"$match": {"ProjectId": {"$binary": "',
        projectIDstr,
        '","$type": "3"}}}, {"$unwind": {"path": "$ExtractionInfo.Sessions"}}]'
      )
    
    studies <- con$aggregate(aggregatestr)
    
    annotationSessions <- studies$ExtractionInfo$Sessions
    
    annotationSessions$StudyIdStr <- sapply(annotationSessions$StudyId,  ConvertRawIdToUUIDString)
    annotationSessions$InvestigatorIdStr <- sapply(annotationSessions$InvestigatorId, ConvertRawIdToUUIDString)
    
    return(annotationSessions)
  }

GetAnnotationsForProject <- function(con, projectID, raw = FALSE)
{
  if (raw == FALSE)
  {
    projectIDstr <- projectID
  } else {
    projectIDstr <-  openssl::base64_encode(projectID)
  }
  
  aggregatestr <-
    paste0(
      '[{"$match": {"ProjectId": {"$binary": "',
      projectIDstr,
      '","$type": "3"}}}, {"$unwind": {"path": "$ExtractionInfo.Annotations"}}]'
    )
  
  studies <- con$aggregate(aggregatestr)
  
  annotations <- studies$ExtractionInfo$Annotations
  annotations$idStr <-
    sapply(annotations$`_id`, ConvertRawIdToUUIDString)
  annotations$StudyIdStr <-
    sapply(annotations$StudyId, ConvertRawIdToUUIDString)
  annotations$InvestigatorIdStr <-
    sapply(annotations$AnnotatorId, ConvertRawIdToUUIDString)
  annotations$QuestionIdStr <-
    sapply(annotations$QuestionId, ConvertRawIdToUUIDString)
  annotations$ParentIdStr <-
    sapply(annotations$ParentId, ConvertRawIdToUUIDString)
  
  annotations$ChildrenIdstrs <-
    sapply(annotations$Children, function(x)
      lapply(x, ConvertRawIdToUUIDString))
  
  return(annotations)
}

FilterAnnotation <- function(annotations, question)
{
  filteredAnnotation <- annotations %>%
    filter(Question == question)
  
  if (nrow(filteredAnnotation) > 0) {
    filteredAnnotation <- filteredAnnotation  %>%
      mutate(Answer = ifelse(is.null(Answer), NULL, unlist(Answer)))
  }
  
  return(filteredAnnotation)
}

GetOutcomes <- function(con)
{
  aggregatestr <-
    paste0(
      '[{"$unwind":  "$ExtractionInfo.OutcomeData"}, {"$unwind": "$ExtractionInfo.OutcomeData.TimePoints"}]'
    )
  
  studies <- con$aggregate(aggregatestr)
  
  outcomeData <- studies$ExtractionInfo$OutcomeData
  
  return(outcomeData)
}

GetOutcomesForProject <- function(con, projectID, raw = FALSE)
{
  
  if (raw == FALSE)
  {
    projectIDstr <- projectID
  } else {
    projectIDstr <-  openssl::base64_encode(projectID)
  }
  
  
  if (raw == FALSE)
  {
    projectIDstr <- projectID
  } else {
    projectIDstr <-  openssl::base64_encode(projectID)
  }
  
  aggregatestr <- paste0(
    '[{
    "$match": {
    "ProjectId": {
    "$binary": "',
    projectIDstr,
    '",
    "$type": "3"}}
}, {"$unwind":  "$ExtractionInfo.OutcomeData"},
    {"$unwind": "$ExtractionInfo.OutcomeData.TimePoints"}
    ]'
  )
  
  studies <- con$aggregate(aggregatestr)
  
  outcomeData <- studies$ExtractionInfo$OutcomeData
  if(!is.null(outcomeData))
  {
    outcomeData$idStr <-sapply(outcomeData$`_id`, ConvertRawIdToUUIDString)
    outcomeData$StageIdStr <-sapply(outcomeData$StageId, ConvertRawIdToUUIDString)
    outcomeData$OutcomeIdStr <-sapply(outcomeData$OutcomeId, ConvertRawIdToUUIDString)
    outcomeData$CohortIdStr <-sapply(outcomeData$CohortId, ConvertRawIdToUUIDString)
    outcomeData$ExperimentIdStr <- sapply(outcomeData$ExperimentId, ConvertRawIdToUUIDString)
    outcomeData$InvestigatorIdStr <-sapply(outcomeData$InvestigatorId, ConvertRawIdToUUIDString)
    outcomeData$ProjectIdStr <- sapply(outcomeData$ProjectId, ConvertRawIdToUUIDString)
    outcomeData$StudyIdStr <- sapply(outcomeData$StudyId, ConvertRawIdToUUIDString)
    
    outcomeData$OutcomeResult <- outcomeData$TimePoints$Average
    outcomeData$OutcomeError <- outcomeData$TimePoints$Error
    outcomeData$TimeInMinute <- outcomeData$TimePoints$Time
    outcomeData$TimePoints <- NULL
  }
  
  return(outcomeData)
}

ConvertRawIdToUUIDString <- function(rawID)
{
  str <- paste0(
    rawID[4],
    rawID[3],
    rawID[2],
    rawID[1],
    "-",
    rawID[6],
    rawID[5],
    "-",
    rawID[8],
    rawID[7],
    "-",
    rawID[9],
    rawID[10],
    "-",
    rawID[11],
    rawID[12],
    rawID[13],
    rawID[14],
    rawID[15],
    rawID[16]
  )
  
  if (str == "----")
    str = NA
  
  return(str)
}

ConverUUIDTotRawIdString <- function(uuid)
{
  uuid <- gsub("-","",uuid)
  rawId <- raw(16)
  
  rawId[1] <- as.raw(strtoi(substr(uuid, 7,8), base=16))
  rawId[2] <- as.raw(strtoi(substr(uuid, 5,6), base=16)) 
  rawId[3] <- as.raw(strtoi(substr(uuid, 3,4), base=16))
  rawId[4] <- as.raw(strtoi(substr(uuid, 1,2), base=16))
  rawId[5] <- as.raw(strtoi(substr(uuid, 11,12), base=16)) 
  rawId[6] <- as.raw(strtoi(substr(uuid, 9,10), base=16)) 
  rawId[7] <- as.raw(strtoi(substr(uuid, 15,16), base=16)) 
  rawId[8] <- as.raw(strtoi(substr(uuid, 13,14), base=16)) 
  
  for(i in 9:16)
  {
    rawId[i] <-  as.raw(strtoi( substr(uuid, i*2-1,i*2), base=16)) 
  }
  
  return(rawId)
}

GetAnnotationsForProjectFull <-
  function(conStudy, conInvestigator, conProject, myProject)
  {
    studies <-
      GetStudiesForProject(conStudy, myProject$`_id`[[1]], raw = TRUE)
    annotations <-
      GetAnnotationsForProject(conStudy, myProject$`_id`[[1]], raw = TRUE)
    investigators <-
      GetInvestigatorsForProject(conInvestigator, conProject, myProject$`_id`[[1]], raw = TRUE)
    
    annotations <- annotations %>%
      left_join(studies[, c("idStr", "Title","Abstract","Author"   ,"Year", "DOI","Journal")], by = c("StudyIdStr" = "idStr")) %>%
      left_join(investigators, by = c("InvestigatorIdStr" = "idStr"))  %>%
      select(
        idStr,
        DateTimeCreated,
        QuestionIdStr,
        Question,
        Root,
        Notes,
        ChildrenIdstrs,
        ParentIdStr,
        Answer,
        StudyIdStr,
        Title,
        Abstract,
        Author,
        Year,
        DOI,
        Journal,
        Investigator,
        InvestigatorIdStr
      )
  }

GetStudyAnnotationsForProjectFull <-
  function(studies, annotations, investigators)
  {
    annotations <- annotations %>%
      left_join(studies[, c("idStr", "Title","Abstract"   , "Authors", "Author" , "Journal"  ,"Year", "DOI")], by = c("StudyIdStr" = "idStr")) %>%
      left_join(investigators, by = c("InvestigatorIdStr" = "idStr"))  %>%
      select(
        idStr,
        DateTimeCreated,
        QuestionIdStr,
        Question,
        Root,
        Notes,
        ChildrenIdstrs,
        ParentIdStr,
        Answer,
        StudyIdStr,
        Title,
        Abstract,
        Author,
        Year,
        Journal,
        DOI,
        Authors,
        Investigator,
        InvestigatorIdStr
      )
  }
