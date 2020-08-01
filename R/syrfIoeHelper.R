#----------  SyRF Nactem Heplers -------------------------------------------------
# Functions supporting converting SyRF data for Nactem API 
# Created by Jing Liao
# Created on May 31 2017
#-------------------------------------------------------------------------

require(dplyr)

RemoveNewline <- function(text){
  # print("-- Start to remove \r \n \f \t")
  text <- gsub("\r|\n|\f|\t|(NULL)", " ", text)
  return(text)
}

CalculateDecisionForIOE <- function(myStudies){
  # include: 1, exclude: 0, unknown: 99
  return( 
    ifelse(myStudies$ScreeningInfo$AgreementMeasure$NumberScreened < 2|
             myStudies$ScreeningInfo$AgreementMeasure$AbsoluteAgreementRatio < 0.5
           , 99
           , ifelse(myStudies$ScreeningInfo$Inclusion > 0.5, 1, 0))
    # ifelse(numberScreened < 2, 99, ifelse(includedCount >= 2, 1, ifelse((numberScreened-includedCount)>=2, 0,99)))
  )
}

CalculateDecisionForIOE <- function(myStudies, myProject){
  # include: 1, exclude: 0, unknown: 99
  return( 
    ifelse(myStudies$ScreeningInfo$AgreementMeasure$NumberScreened < myProject$AgreementThreshold$NumberScreened |
             myStudies$ScreeningInfo$AgreementMeasure$AbsoluteAgreementRatio < myProject$AgreementThreshold$AbsoluteAgreementRatio
           , 99
           , ifelse(myStudies$ScreeningInfo$Inclusion > 0.5, 1, 0))
    # ifelse(numberScreened < 2, 99, ifelse(includedCount >= 2, 1, ifelse((numberScreened-includedCount)>=2, 0,99)))
          )
}

ExtractDataForIoeAPI <- function(studies){
  # if(!("Label" %in% names(studies))) CalculateDecisionForIOE(studies, project)
  studies$Keywords <- sapply(studies$Keywords, paste0, collapse = ", ")
  
  outputDataNactem <- studies %>%
    select(
      LABEL = Label,
      ITEM_ID = idStr,
      REVIEW_ID = ProjectIdStr,
      TITLE = Title,
      KEYWORDS = Keywords,
      ABSTRACT = Abstract,
      Cat = Cat)
  
  return(outputDataNactem)
}

CreateMLFilenames <- 
  function(outputFolder, runningId){
    dir.create(outputFolder, showWarnings = F)
    
  list(
    Records  = paste(outputFolder, runningId, "_records", ".csv", sep = ""),
    Decisions = paste(outputFolder, runningId, "_decision", ".csv", sep = ""),
    Vectors = paste(outputFolder, runningId, "_vectors", ".csv", sep = ""),
    TestDecisions = paste(outputFolder, runningId, "_testDecision", ".csv", sep = ""),
    AllDecisions = paste(outputFolder, runningId, "_allDecision", ".csv", sep = ""),
    Results = paste(outputFolder, runningId, "_result", ".csv", sep = ""),
    Analysis = paste(outputFolder, runningId, "_analysis", ".csv", sep = ""),
    SyRFUploadFile = paste(outputFolder, runningId, "_SyRFUploadFile", ".txt", sep = "")
  )
    
}

# Preapre the training data and testing data
WriteFilesForIOE <-
  function(myData, outputFilenames, testPercentage = 0.2, randomSeed = 42,
           decisionLabel = c(0,1), noDecisionLabel = 99){
    myData <- myData %>%
      mutate(
        REVIEW_ID = RemoveNewline(REVIEW_ID),
        ITEM_ID = RemoveNewline(ITEM_ID),
        TITLE = RemoveNewline(TITLE),
        ABSTRACT = RemoveNewline(ABSTRACT),
        KEYWORDS = RemoveNewline(KEYWORDS)
      )
    
    validRecords <- myData %>%
      select(
        REVIEW_ID ,
        ITEM_ID,
        TITLE,
        ABSTRACT ,
        KEYWORDS
      )
    
    validDecisions <- myData %>%
      select(  REVIEW_ID,
               ITEM_ID ,
               LABEL
      )

    DivideDataset <- function(myData, testPercentage, randomSeed,decisionLabel, noDecisionLabel){
      set.seed(randomSeed)
      
      numberData <- length(which(myData$LABEL %in% decisionLabel))
      numberTestData <- ceiling(numberData * testPercentage)
      numberTrainData <- numberData - numberTestData

      indexTestLabel <- which((myData$LABEL %in% decisionLabel) & myData$Cat == "Test")
      indexTrainLabel <- which((myData$LABEL %in% decisionLabel) & myData$Cat == "Train")
        
      numberTestDataToRandom <- max(numberTestData - length(indexTestLabel),0)
      
      indexDecision <- which(myData$LABEL %in% decisionLabel)
      indexToRandom <- setdiff(indexDecision, c(indexTestLabel, indexTrainLabel))
      
      indexTest <- c(indexTestLabel, sample(indexToRandom, numberTestDataToRandom, replace=F))
      indexTrain <- c(indexTrainLabel, setdiff(indexToRandom, indexTest))
      
      return(list(indexTest = indexTest, indexTrain = indexTrain))
    }
  
    indexSets <- DivideDataset(myData, testPercentage = testPercentage, randomSeed = randomSeed,
                               decisionLabel = decisionLabel, noDecisionLabel = noDecisionLabel)
    indexTest <- indexSets$indexTest
    indexTrain <- indexSets$indexTrain
    
    validDecisionsTrain <- validDecisions
    validDecisionsTrain$LABEL[indexTest] <- noDecisionLabel
    validDecisionsTest <- validDecisions[indexTest,]
    
    write.csv(
      validRecords,
      file = outputFilenames$Records,
      row.names = F,
      quote = T
    )
    write.csv(
      validDecisionsTrain,
      file = outputFilenames$Decisions,
      row.names = F,
      quote = T
    )
    write.csv(
      validDecisionsTest,
      file = outputFilenames$TestDecisions,
      row.names = F,
      quote = T
    )
    
    validDecisions$Category = "Unknown"
    validDecisions$Category[indexSets$indexTest] = "Test"
    validDecisions$Category[indexSets$indexTrain] = "Train"
    
    write.csv(
      validDecisions,
      file = outputFilenames$AllDecisions,
      row.names = F,
      quote = T
    )
    
    return(validDecisions)
  }

ReformDataForSyRF <- function(myStudies, outputFilenameResultData, myData, threshold){
  outputFilenameResultData$MLInclusion <- outputFilenameResultData$score > threshold
  myData$HMInclusion <- myData$LABEL
  
  mergedData <- merge(outputFilenameResultData, myData)
  
  myStudies <- myStudies[ , !(names(myStudies) %in%  c("PublicationName", "ScreeningInfo", "ExtractionInfo"))]
  
  mergedData <- merge(mergedData, myStudies, by.x = "ITEM_ID", by.y = "idStr")

  outputData <- mergedData %>%
    mutate(
      title = RemoveNewline(Title),
      surname = Author,
      firstname = "",
      csurname = "",
      cfirstname = "",
      cauthororder = "",
      publicationName = RemoveNewline(Journal),
      doi = RemoveNewline(DOI),
      url = "",
      abstract = RemoveNewline(Abstract),
      keywords = ITEM_ID,
      URL = "",
      authorAddress = "",
      referenceType = ReferenceType,
      pdfPath = "",
      includedHM = as.numeric(HMInclusion),
      includedML = as.numeric(MLInclusion),
      score = score
    ) %>%
    select(
      Title = title,
      "First Author First Name" = firstname,
      "First Author Surname" = surname,
      "Corresponding Author First Name" = cfirstname,
      "Corresponding Author Surname" = csurname,
      "Corresponding Author Order" = cauthororder,
      "Publication Name" = publicationName,
      "Alternate Name" = Journal,
      Abstract = abstract,
      Url = URL,
      "Author Address" = authorAddress,
      Year = Year,
      DOI = doi,
      "Reference Type" = referenceType,
      "PDF Relative Path" = pdfPath,
      Keywords = keywords,
      includedHM = includedHM,
      includedML  = includedML,
      score = score
    )
  
  return(outputData)
}


WriteFilesForSyRF <-
  function(myStudies, outputFilenameResultData, myData, threshold, outputFilenameSyRF, outputType ="Hybrid"){
    resultData <- ReformDataForSyRF(myStudies, outputFilenameResultData, myData, threshold)
    
    resultData$included <- switch(outputType, "HumanOnly" = resultData$includedHM,
                                  "MachineOnly" = resultData$includedML,
                                  "Hybrid" = ifelse(resultData$includedHM != 99, resultData$includedHM, resultData$includedML))
    
    resultData[, "included"] = resultData$included
    
    outputData <- resultData[, c(
      "Title" ,"First Author First Name"  , "First Author Surname" ,"Corresponding Author First Name" ,"Corresponding Author Surname"  ,"Corresponding Author Order"   , "Publication Name"    , "Alternate Name"  , "Abstract"   ,"Url"  ,"Author Address" , "Year"  ,"DOI" , "Reference Type"  , "PDF Relative Path"     , "Keywords"  , "included"
    )]
    
    write.table(
      outputData,
      file = outputFilenameSyRF,
      append = FALSE,
      sep = "\t",
      row.names = F,
      col.names = T,
      quote = T,
      na = ""
    )
    
    return(outputData)
  }

WriteFilesForSyRFErrorAnalysis <-
  function(myStudies, outputFilenameResultData, myData, threshold, outputFilenameSyRF, numberDownload = 200){
  
    resultData <- ReformDataForSyRF(myStudies, outputFilenameResultData, myData, threshold)
    resultData$Id <- rownames(resultData)
    
    resultData <- resultData %>%
      mutate(
         absScore = ifelse(score > threshold,  abs(score - threshold)/(1-threshold), abs(score - threshold)/threshold)
             )

    resultDataChange <- resultData %>%
      filter(!is.null(includedHM) & includedHM != 99  & includedHM != includedML) %>%
    top_n(numberDownload , wt = absScore)
    
    resultData <- resultData %>%
      filter(!is.null(includedHM) & includedHM != 99) %>%
      mutate(
        included = ifelse(Id %in% resultDataChange$Id, includedML, includedHM)
      )
    
    names(resultData)[which(names(resultData) == "includedHM")] = "DecisionD20B5387-E6A8-4722-8A03-956452790272"
    names(resultData)[which(names(resultData) == "included")] = "Decision5DFF96A2-71AF-48E1-A92A-EB6AEFBF7BAE"
    
    outputData <- resultData[, c(
      "Title" ,"First Author First Name"  , "First Author Surname" ,"Corresponding Author First Name" ,"Corresponding Author Surname"  ,"Corresponding Author Order"   , "Publication Name"    , "Alternate Name"  , "Abstract"   ,"Url"  ,"Author Address" , "Year"  ,"DOI" , "Reference Type"  , "PDF Relative Path"     , "Keywords"  , "DecisionD20B5387-E6A8-4722-8A03-956452790272"
      ,"Decision5DFF96A2-71AF-48E1-A92A-EB6AEFBF7BAE"
    )]
    
    write.table(
      outputData,
      file = outputFilenameSyRF,
      append = FALSE,
      sep = "\t",
      row.names = F,
      col.names = T,
      quote = F,
      na = ""
    )
    return(outputData)
  }

PlotPerformance <- function(analysisResult, graphnames,projectName){
  # analysisResult <- analysisResult %>%
  #   gather("Type", "Performance", -c(Chosen, Thresholds))
  chosenThresholds <- analysisResult$Thresholds[which(analysisResult$Chosen)][1]
  
  myPlot <- ggplot(data=analysisResult, aes(x=FPR, y=Sensitivity
                                            # , group=Type
                                            # , colour=Type
                                            , title = paste0(projectName, " ROC"))) + geom_line() + geom_vline(xintercept=chosenThresholds )+ ggtitle(projectName)
  
  ggsave( basename(graphnames), path = dirname(graphnames),plot = myPlot, device= png(), width = 8, height = 4)
}
