#---------- Helpers Aiding analyze -------------------------------------------------
# Created by Jing Liao
# Created on May 31 2017
#-------------------------------------------------------------------------

require(caret)
require(e1071)
CalcualteCM <- function(mergedResult, threshold)
{
  options(warn=-1)
  data <- factor(mergedResult$Score > threshold, levels = c("TRUE", "FALSE"))
  ref <- factor(mergedResult$HumanDecision == 1, levels = c("TRUE", "FALSE"))
  confusionMatrix(data
                  , ref
                  , positive = "TRUE")
}

CalculateMLPerformance <- function(outputFilenameResult, outputFilenameTestDecisions, AnalysisFileName, minimumSensitivity = 0.95)
{
  n <- 100
  thresholds <- 0+0.01*(1:n)
  utility <- array(NA,n)
  Sensitivity <- array(NA,n)
  Specificity <- array(NA,n)
  Precision <- array(NA,n)
  
  mlResult <- read.csv(outputFilenameResult, header = F, col.names = c("Score", "ID", "ProjectID"))
  testResult <- read.csv(outputFilenameTestDecisions, header = T,col.names = c("ProjectID", "ID", "HumanDecision"))
  
  mergedResult <- merge(mlResult, testResult)
  print(dim(mergedResult))
  mergedResult <- mergedResult[which(!is.na(mergedResult$HumanDecision) & !is.na(mergedResult$Score) ),]

  for(i in 1:n)
  {
    threshold <- thresholds[i]
    
    cm <- CalcualteCM(mergedResult, threshold)
    # print(paste(threshold,cm[4]$byClass["Sensitivity"],cm[4]$byClass["Specificity"],cm[4]$byClass["Precision"] )  )
    utility[i] <-  (5*cm[4]$byClass["Sensitivity"] + cm[4]$byClass["Specificity"])/(1+5)
    Specificity[i] <- cm[4]$byClass["Specificity"]
    Sensitivity[i] <- cm[4]$byClass["Sensitivity"]
    Precision[i] <- cm[4]$byClass["Precision"]
  }
  
  index <- which( Specificity == max(Specificity[which(Sensitivity > minimumSensitivity)]))
  
  finalThreshold <- thresholds[index[1]]
  chosen <- rep(FALSE, length(thresholds))
  chosen[index[1]] <- TRUE
  
  print(paste0("best treshold is: ", finalThreshold))
  
  finalCM <- CalcualteCM(mergedResult, finalThreshold)
  print(finalCM)
  
  results <- data.frame(Thresholds = thresholds, Specificity = Specificity
                        , Sensitivity =Sensitivity, Precision = Precision
                        , Chosen = chosen)
  
  write.csv(results, file = AnalysisFileName)
  capture.output(finalCM, file = AnalysisFileName,  type = c("output", "message"), append = T)
  
  results$FPR <- (1 - results$Specificity)
  
  
  return(results)
}
