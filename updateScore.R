source("R/syrfDBReader.R")
source('R/ioeAPI.R')
source('R/syrfIoeHelper.R')
source('R/calculateMLPerformance.R')
source('R/dictionaryValidationHelpers.R')
source("R/functions.R")
source('R/configure.R')
source('R/relisyrScoring.R')

library(googlesheets4)
library(AutoAnnotation)
library(DBI)
library(RMySQL)
library(tidyr)
library(dplyr)

print(paste0(' --------- ', date(), ' --------- '))
setwd("/home/jliao/workspace/DIST")

updateScore()

CreateProgressSummary(syrfConnection, clinicalProjectId, invivoProjectId,
                                  mySQLCon, clinicalSQLTableName,invivoSQLTableName,
                                  googleSheetId,
                                  diseaseOfInterestSheetName = diseaseOfInterestSheetName,
                                  drugOfInterestSheetName = drugOfInterestSheetName,
                                  diseaseSheetName = diseaseSheetName,
                                  drugSheetName = drugSheetName,
                                  scoreRuleSheetName = scoreRuleSheetName,
                                  progressSummarySheetName = progressSummarySheetName,
                                  clinicalProjectProgresRangeLine = clinicalProjectProgresRangeLine,
                                  invivoProjectProgresRangeLine = invivoProjectProgresRangeLine)
