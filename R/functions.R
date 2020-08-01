 


create_file_folders <- function(inputFolder = "data/", outputFolder = "output/"){
  df <- list(
    InputFolder = inputFolder,
    OutputFolder = outputFolder 
  )
  dir.create(df$InputFolder, showWarnings = F)
  dir.create(df$OutputFolder, showWarnings = F)
  
  return(df)
}

has_new_clinical_study <-
  function(syrfConnection,
           updatedProjectIdBase64,
           latestpdateDate) {
    return(
      HasStudyForProject(
        syrfConnection$conStudy,
        updatedProjectIdBase64,
        latestpdateDate
      )
    )
  }

count_new_clinical_study <-
  function(syrfConnection,
           updatedProjectIdBase64,
           latestpdateDate) {
    return(
      CountStudyForProject(syrfConnection$conStudy,
                           updatedProjectId,
                           latestpdateDate)
    )
  }

load_syrf_studies_for_project <-
  function(syrfConnection, projectId) {
    project <-
      GetProject(syrfConnection$conProject, projectId, raw = FALSE)
    
    studies <-
      GetStudiesForProject(syrfConnection$conStudy, projectId, raw = FALSE) %>%
      mutate(Cat = "")
    
    studies$Label <-
      CalculateDecisionForIOE(studies, project)
    
    return(studies)
  }

screening <-
  function(syrfConnection,
           myProjectId,
           errorAnalysisProjectId,
           fileFolder = list(InputFolder = "data/", OutputFolder = "output/") ,
           version = "v1",
           onlyTrainWithValidAbstract = F) {
    # --- 0. machine leaning log ----
    outputMLFilenames <-
      CreateMLFilenames(paste0(fileFolder$OutputFolder, "MLoutput/"), version)
    
    # --- 1. Read data from database ----
    myStudies <-
      load_syrf_studies_for_project(syrfConnection, myProjectId) %>%
      mutate(NewIdStr = idStr)
    indexHistorical <-
      which(!is.na(as.numeric(myStudies$ReferenceType)))  #ReferenceType records HistoricalId
    if (length(indexHistorical) > 0)
      myStudies$idStr[indexHistorical] <-
      sapply(myStudies$Keywords[indexHistorical],  unlist)
    myStudies <- myStudies %>%
      mutate(AltJournal = Journal) %>%
      select(
        Label,
        idStr,
        ProjectIdStr,
        Title,
        Keywords,
        Abstract,
        Cat,
        Authors,
        Journal,
        DOI,
        Author,
        Year,
        AltJournal,
        ReferenceType,
        NewIdStr,
        DateTimeCreated
      )
    
    # --- 2. run through systematic search uploads in error analysis to update the decsions ----
    if (errorAnalysisProjectId != "") {
      errorAnalysisProject <-
        GetProject(syrfConnection$conProject, errorAnalysisProjectId, raw = FALSE)
      
      errorAnalysisStudies <-
        load_syrf_studies_for_project(syrfConnection, errorAnalysisProjectId)
      errorAnalysisStudies$OldStudyIdStr <-
        sapply(errorAnalysisStudies$Keywords, unlist)
      
      myStudies$Label.initial <- myStudies$Label
      iis <-
        seq_along(errorAnalysisProject$SystematicSearchIdStrs[[1]])
      for (ii in iis) {
        isearchIstr <- errorAnalysisProject$SystematicSearchIdStrs[[1]][ii]
        newErrorLabel <- paste0("Label", ".e", ii)
        imyErrorAnalysisStudies <-
          errorAnalysisStudies[which(errorAnalysisStudies$SystematicSearchIdStr == isearchIstr),]
        imyErrorAnalysisStudies$TotalCount <-
          imyErrorAnalysisStudies$ScreeningInfo$IncludedCount / imyErrorAnalysisStudies$ScreeningInfo$Inclusion
        # As the design of error analysis, we set anything without a error analysis decision as no decision (NA) in error analysis study
        imyErrorAnalysisStudies$Label[imyErrorAnalysisStudies$TotalCount != 3] <-
          NA
        imyErrorAnalysisStudies[, newErrorLabel] <-
          imyErrorAnalysisStudies$Label
        
        myStudies <-
          merge(
            myStudies,
            imyErrorAnalysisStudies[, c("OldStudyIdStr", newErrorLabel)],
            by.x = "idStr",
            by.y = "OldStudyIdStr",
            all.x = T
          )
        
        myStudies$Label <- ifelse(is.na(myStudies[, newErrorLabel])
                                  | myStudies[, newErrorLabel] == 99
                                  ,
                                  myStudies$Label,
                                  myStudies[, newErrorLabel])
      }
    }
    
    if (onlyTrainWithValidAbstract)
      myStudies$Cat[which(myStudies$Abstract == "")] <- "Test"
    
    #---- 3. Process data for IOE API ----
    #Extract data from studies. 0: excluded, "": unknown, 1: included
    ioeApiData <- ExtractDataForIoeAPI(myStudies)
    
    ioeApiDataDivided <-
      WriteFilesForIOE(ioeApiData, outputMLFilenames)
    
    # --- 4. Send the data to IOE API and Write the results  ----
    filenames <-
      CreateFileNamesForIOEAPI(
        outputMLFilenames$Records,
        outputMLFilenames$Decisions,
        outputMLFilenames$Vectors ,
        outputMLFilenames$Results
      )
    
    TrainCollection(filenames,
                    gsub("[-]", "", myStudies$ProjectIdStr[1]),
                    forceVectorization = F)
    # --- 5. Analyse training results ----
    analysisResult <-
      CalculateMLPerformance(
        outputMLFilenames$Results,
        outputMLFilenames$TestDecisions,
        outputMLFilenames$Analysis,
        minimumSensitivity = 0.945
      )
    #---- 6. Convert it for uploading to SyRF (different project) ----
    results <-
      read.csv(
        filenames$ResultsFileName,
        header = FALSE,
        col.names = c("score", "ITEM_ID", "REVIEW_ID")
      )
    
    syrfOutput <- WriteFilesForSyRF(
      myStudies,
      results,
      ioeApiData,
      analysisResult$Thresholds[which(analysisResult$Chosen)]
      ,
      outputMLFilenames$SyRFUpload
      ,
      outputType = "Hybrid"
    )  #"HumanOnly" , MachineOnly,  Hybrid
    
    combinedUniqueLabeledStudies <- syrfOutput %>%
      left_join(myStudies[, c("idStr", "NewIdStr", "ProjectIdStr", "DateTimeCreated")], by = c("Keywords" = "idStr")) %>%
      select(
        Title,
        Author = "First Author Surname",
        Journal = "Publication Name",
        Abstract,
        Year,
        OldIdStr = Keywords,
        HistoricalID = "Reference Type",
        PublicationID = DOI,
        Label = included,
        NewIdStr,
        ProjectIdStr,
        DateTimeCreated
      )
    
    return(combinedUniqueLabeledStudies)
  }

# 
CreateProgressSummary <- function(syrfConnection, clinicalProjectId,invivoProjectId,
                                  mySQLCon, clinicalSQLTableName,invivoSQLTableName,
                                  googleSheetId,
                                  diseaseOfInterestSheetName = "diseaseOfInterest",
                                  drugOfInterestSheetName = "drugOfInterest",
                                  diseaseSheetName = "diseaseDictionary",
                                  drugSheetName = "drugDictionary",
                                  scoreRuleSheetName = "ScoreRule",
                                  progressSummarySheetName,
                                  clinicalProjectProgresRangeLine = clinicalProjectProgresRangeLine,
                                  invivoProjectProgresRangeLine = invivoProjectProgresRangeLine){
  
  clinicalSyRFStudies <- GetStudiesForProject(syrfConnection$conStudy, clinicalProjectId, raw = FALSE)
  invivoSyRFStudies <- GetStudiesForProject(syrfConnection$conStudy, invivoProjectId, raw = FALSE)
  
  clinicalProject <- GetProject(syrfConnection$conProject, clinicalProjectId , raw=F)
  clinicalAnnotations <- GetAnnotationsForProjectFull(syrfConnection$conStudy, syrfConnection$conInvestigator, syrfConnection$conProject, clinicalProject)
  clinicalAnnotations <- CleanAnnotations(clinicalAnnotations)
  latestDatetime <- max(clinicalAnnotations$DateTimeCreated)
  
  clinicalStudyList <- RMySQL::dbReadTable(mySQLCon, clinicalMySQLTableName) 
  invivoStudyList <- RMySQL::dbReadTable(mySQLCon, invivoMySQLTableName)
  
  diseaseOfInterest  <- (googlesheets4::read_sheet(googleSheetId, sheet = diseaseOfInterestSheetName))$diseaseOfInterest
  drugOfInterest <- (googlesheets4::read_sheet(googleSheetId, sheet = drugOfInterestSheetName))$drugOfInterest

  scoreRule <-
    googlesheets4::read_sheet(googleSheetId, sheet = scoreRuleSheetName)
  scoreRule$QuestionUnique <- paste0(scoreRule$QuestionIdStr, "_",scoreRule$Question)
  
  crossTable <- as.data.frame.matrix(table(clinicalStudyList[,c("Drug","Disease")]))
  chosenDiseases <- "MND"
  crossTable$select  <- F
  for(chosenDiease in chosenDiseases){
    crossTable$score1 <- rowSums(crossTable[, chosenDiease, drop = F])
    crossTable$score2 <- rowSums(crossTable[, setdiff(diseaseOfInterest, chosenDiease), drop=F] > 0)
    crossTable$select <- crossTable$select | (crossTable$score1 > 0 | crossTable$score2 >= 2)
  }
  crossTable <- crossTable[which(crossTable$select), ]
  
  clinicalDrugMeetLogic <- rownames(crossTable[which(crossTable$select), ])
  nClinicalDrugMeetLogic = length(clinicalDrugMeetLogic)
  nClinicalPublicationsMeetLogic <- length(which(clinicalStudyList$Drug %in%  clinicalDrugMeetLogic))
  
  clinicalCoreDrugs <- intersect(drugOfInterest, clinicalDrugMeetLogic)
  nClinicalCoreDrugs = length(clinicalCoreDrugs)
  nClinicalCoreDrugPublications <- length(which(clinicalStudyList$Drug %in%  clinicalCoreDrugs))
  
  validStudyInvestigatorIdStr <- clinicalAnnotations %>%
    filter(QuestionIdStr == unique(scoreRule$QuestionIdStr[scoreRule$ScoreType == "Efficacy"])) %>%
    arrange(DateTimeCreated) %>%
    group_by(
      StudyIdStr
    ) %>%
    summarise(
      nReviewers = length(unique(InvestigatorIdStr))
    )
  
  clinicalProgressSummary <- data.frame(
    ReviewType = "Clinical",
    nUniquePublications = nrow(clinicalSyRFStudies),
    nIncludedPublications = nrow(clinicalStudyList),
    nDrugMeetLogic = nClinicalCoreDrugs,
    nPublicationsMeetLogic = nClinicalPublicationsMeetLogic,
    longListLatestUpdatedDate = format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000Z"),
    nCoreDrugs = nCoreDrugs,
    nCoreDrugPublications = nClinicalCoreDrugPublications,
    nSingleAnnotated = table(validStudyInvestigatorIdStr$nReviewers)["1"],
    nDualAnnotated = table(validStudyInvestigatorIdStr$nReviewers)["2"],
    nReconciled = 0,
    ReviewLatestUpdatedDate = latestDatetime
  )
  
  invivoStudiesMeetLogic <- invivoStudyList[invivoStudyList$Drug %in% clinicalDrugMeetLogic,] %>%
    group_by(idStr) %>%
    summarise(Title = first(Title),
              Author = first(Author),
              Journal = first(Journal),
              Abstract = first(Abstract),
              Year = first(Year),
              Disease = paste0(Disease, collapse = "; "),
              Drug = paste0(Drug, collapse = "; ")
    )
  invivoDrugMeetLogic <- unique(invivoStudiesMeetLogic$Drug)

  invivoStudiesCoreDrugs <- invivoStudyList[invivoStudyList$Drug %in% clinicalCoreDrugs,] %>%
    group_by(idStr) %>%
    summarise(Title = first(Title),
              Author = first(Author),
              Journal = first(Journal),
              Abstract = first(Abstract),
              Year = first(Year),
              Disease = paste0(Disease, collapse = "; "),
              Drug = paste0(Drug, collapse = "; ")
    )
  invivoDrugCoreDrugs <- unique(invivoStudiesMeetLogic$Drug)
  
  invivoProgressSummary <- data.frame(
    ReviewType = "In vivo",
    nUniquePublications = nrow(invivoSyRFStudies),
    nIncludedPublications = nrow(invivoStudyList),
    nDrugMeetLogic = length(invivoDrugMeetLogic),
    nPublicationsMeetLogic = nrow(invivoStudiesMeetLogic),
    longListLatestUpdatedDate = format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000Z"),
    nCoreDrugs = nCoreDrugs,
    nCoreDrugPublications = nCoreDrugPublications,
    nSingleAnnotated = table(validStudyInvestigatorIdStr$nReviewers)["1"],
    nDualAnnotated = table(validStudyInvestigatorIdStr$nReviewers)["2"],
    nReconciled = 0,
    ReviewLatestUpdatedDate = latestDatetime
  )
  
  invitroProgressSummary <- data.frame(
    ReviewType = "In vitro",
    nUniquePublications = 0,
    nIncludedPublications = 0,
    nDrugMeetLogic = 0,
    nPublicationsMeetLogic = 0,
    longListLatestUpdatedDate = format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000Z"),
    nCoreDrugs = 0,
    nCoreDrugPublications = 0,
    nSingleAnnotated = 0,
    nDualAnnotated = 0,
    nReconciled = 0,
    ReviewLatestUpdatedDate = latestDatetime
  )
  
  googlesheets4::sheet_write(rbind(clinicalProgressSummary, invivoProgressSummary, invitroProgressSummary), googleSheetId, sheet = progressSummarySheetName)
  
}

# produce data function
produceData <-
  function(syrfConnection,
           myProjectId,
           errorAnalysisProjectId = "",
           fileFolder = list(InputFolder = "data/", OutputFolder = "output/") ,
           version = "v1",
           mySQLCon,
           mySQLTableName,
           googleSheetId,
           logSheetName,
           diseaseSheetName = "diseaseDictionary",
           drugSheetName = "drugDictionary",
           projectSummaryGoogleSheetId = "1-TRbnLo4ypCdlkhsWCRh-wVBXwtwbpPm0EhjfaesNhQ",
           progressSummarySheetName = "progressSummary",
           progressSummarySheetRangeLine           ) {
    # 2.1 run machine learning to update screening
    newSyRFStudies <-
      screening(
        syrfConnection = syrfConnection,
        myProjectId = myProjectId,
        errorAnalysisProjectId = errorAnalysisProjectId,
        fileFolder = fileFolder,
        version = version,
        onlyTrainWithValidAbstract = F
      )
    
    print(paste0(nrow(newSyRFStudies), " studies."))
    # 2.2 check whether there is new included sutdy
    newIncludedSyRFStudies <-
      newSyRFStudies[which(newSyRFStudies$Label == 1),]
    
    print(paste0(nrow(newIncludedSyRFStudies), " included study."))
    # 2.3 extract drug and disease for new included studies
    diseaseDictionary <-
      googlesheets4::read_sheet(googleSheetId, sheet = diseaseSheetName)
    drugDictionary <-
      googlesheets4::read_sheet(googleSheetId, sheet = drugSheetName)
    newStudyList <-
      ExtractDrugDisease(
        as.data.frame(newIncludedSyRFStudies),
        diseaseDictionary = as.data.frame(diseaseDictionary),
        drugDictionary = as.data.frame(drugDictionary)
        ,
        idColumn = "OldIdStr",
        minimumIncludeFrequency = 9,
        maximumExcludeFrequency = 1,
        groupAnnotation = F
      )
    myProject <-
      GetProject(syrfConnection$conProject, myProjectId, raw = F)
    stageIdStr <-
      ConvertRawIdToUUIDString(last(myProject$Stages[[1]]$`_id`))
    newStudyList$Link <- paste0(
      "http://app.syrf.org.uk/projects/",
      myProject$idStr ,
      "/stage/",
      stageIdStr ,
      "/review/",
      newStudyList$NewIdStr,
      ";reconciliation=false"
    )
    
    newStudyList$idStr <- newStudyList$NewIdStr
    # ---- 3. Write out the new clinical studies and new logs
    currentStudyList <-
      RMySQL::dbReadTable(mySQLCon, mySQLTableName)
    columnsNames <- names(currentStudyList)
    # write out data to google sheet
    RMySQL::dbWriteTable(
      mySQLCon,
      value = as.data.frame(newStudyList[, columnsNames]),
      name = mySQLTableName,
      append = F,
      overwrite = T,
      row.names = FALSE
    )
    
    # write out new update on log to google sheet
    log <- googlesheets4::read_sheet(googleSheetId, sheet=logSheetName)

    newlog <-
      rbind(
        log,
        data.frame(
          UpdateTime = format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000Z"),
          Updated = T,
          NIncluded = nrow(newStudyList),
          NTotal = nrow(newSyRFStudies),
          Comments = "reproduce data"
        )
      )   %>%
      arrange(UpdateTime)
    googlesheets4::write_sheet(newlog, googleSheetId, sheet = logSheetName)
    
  }

updateData <-
  function(syrfConnection,
           myProjectId,
           errorAnalysisProjectId = "",
           fileFolder = list(InputFolder = "data/", OutputFolder = "output/") ,
           version = "v1",
           mySQLCon,
           mySQLTableName,
           googleSheetId,
           logSheetName,
           diseaseSheetName = "diseaseDictionary",
           drugSheetName = "drugDictionary",
           progressSummarySheetName = "progressSummary",
           progressSummarySheetRangeLine
           ) {
    # ---- 1. Check weather there is new studies
    log <- googlesheets4::read_sheet(googleSheetId, sheet= logSheetName)
    latestpdateDate <- max(log$UpdateTime)
    
    if (!has_new_clinical_study(syrfConnection, myProjectId, latestpdateDate)) {
      newlog <-
        rbind(log,
              data.frame(
                UpdateTime = format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000Z"),
                Updated = F,
                NIncluded = "",
                NTotal = "",
                Comments = "Routine update"
              )) %>%
        arrange(UpdateTime)
      googlesheets4::sheet_write(newlog, googleSheetId, sheet = logSheetName)
      return(FALSE)
    }
    
    # 2.1 run machine learning to update screening
    newSyRFStudies <-
      screening(
        syrfConnection = syrfConnection,
        myProjectId = myProjectId,
        errorAnalysisProjectId = errorAnalysisProjectId,
        fileFolder = fileFolder,
        version = version,
        onlyTrainWithValidAbstract = F
      )
    
    print(paste0(nrow(newSyRFStudies), " studies."))
    # 2.2 check whether there is new included sutdy
    currentStudyList <-
      RMySQL::dbReadTable(mySQLCon, mySQLTableName)
    
    newIncludedSyRFStudies <-
      newSyRFStudies[which(
        !newSyRFStudies$OldIdStr %in% unique(currentStudyList$OldIdStr) &
          newSyRFStudies$Label == 1
      ), ]
    
    if (nrow(newIncludedSyRFStudies) == 0) {
      newlog <-
        rbind(log,
              data.frame(
                UpdateTime = format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000Z"),
                Updated = F,
                NIncluded = nrow(currentStudyList),
                NTotal = nrow(newSyRFStudies)
              )) %>%
        arrange(UpdateTime)
      googlesheets4::sheets_write(newlog, googleSheetId, sheet = logSheetName)
      return(FALSE)
    }
    
    print(paste0(nrow(newIncludedSyRFStudies), " new included study."))
    
    # 2.3 extract drug and disease for new included studies
    diseaseDictionary <-
      googlesheets4::read_sheet(googleSheetId, sheet = diseaseSheetName)
    drugDictionary <-
      googlesheets4::read_sheet(googleSheetId, sheet = drugSheetName)
    newStudyList <-
      ExtractDrugDisease(
        as.data.frame(newIncludedSyRFStudies),
        diseaseDictionary = as.data.frame(diseaseDictionary),
        drugDictionary = as.data.frame(drugDictionary)
        ,
        idColumn = "OldIdStr",
        minimumIncludeFrequency = 9,
        maximumExcludeFrequency = 1,
        groupAnnotation = F
      )
    
    newStudyList$HistoricalID[newStudyList$HistoricalID == "Journal Article"] = NA
    
    myProject <-
      GetProject(syrfConnection$conProject, myProjectId, raw = F)
    stageIdStr <-
      ConvertRawIdToUUIDString(last(myProject$Stages[[1]]$`_id`))
    newStudyList$Link <- paste0(
      "http://app.syrf.org.uk/projects/",
      myProject$idStr ,
      "/stage/",
      stageIdStr ,
      "/review/",
      newStudyList$NewIdStr,
      ";reconciliation=false"
    )
    
    newStudyList$idStr <- newStudyList$NewIdStr
    
    # ---- 3. Write out the new clinical studies and new logs
    # write out data to google sheet
    columnsNames <- names(currentStudyList)
    
    RMySQL::dbWriteTable(
      MySQLCon,
      value = as.data.frame(newStudyList[, columnsNames]),
      name = mySQLTableName,
      append = F,
      overwrite = T,
      row.names = FALSE
    )
    
    # write out new update on log to google sheet
    newlog <-
      rbind(
        log,
        data.frame(
          UpdateTime = format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000Z"),
          Updated = T,
          NIncluded = nrow(newStudyList) + nrow(currentStudyList),
          NTotal = nrow(newSyRFStudies),
          Comments = ""
        )
      )   %>%
      arrange(UpdateTime)
    googlesheets4::write_sheet(newlog, googleSheetId, sheet = logSheetName)
    
    return(TRUE)
  }
