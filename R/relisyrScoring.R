source('R/syrfDBReader.R')
source('R/syrfDataExtractors.R')
source('R/scoreFunctions.R')
source('R/configure.R')
library(dplyr)
library(tidyr)

updateScore <- function(){
  
# ---- 0. Load score rule ----
scoreRule <-
  googlesheets4::read_sheet(googleSheetId, sheet = scoreRuleSheetName)
scoreRule$QuestionUnique <- paste0(scoreRule$QuestionIdStr, "_",scoreRule$Question)

efficacyQuestionIdStr <- unique(scoreRule$QuestionUnique[scoreRule$ScoreType == "Efficacy"])
studySizeQuestionIdStr <- unique(scoreRule$QuestionUnique[scoreRule$ScoreType == "Study Size"])
safteyQuestionIdStr <- unique(scoreRule$QuestionUnique[scoreRule$ScoreType == "Safety"])
qualityQuestionIdStr <- unique(scoreRule$QuestionUnique[scoreRule$ScoreType == "Quality"])
diseaseQuestionIdStr <- "53068292-10e1-4019-8ed8-b1f754da192a_What is the disease?"
interventionIdStr <- "60a86326-8aa0-485d-9f8f-2e9d508b932d_What is the intervention?"
nAnimalIdStr <- "83caa64f-86a1-4f6e-a278-ebbd25297677_Number of animals in cohort"
blindingIdStr <- "9c21037f-61b2-40b7-813d-3f4d9a7769dd_Blinded assessment of outcome"
allRelevantQuestionUnique <- c(scoreRule$QuestionUnique, diseaseQuestionIdStr,interventionIdStr)

drugSummarySheetName <- "DrugSummary"
publicationListSheetName <- "PublicationList"
ProgressSummarySheetName <- "ProgressSummary"

# ---- 1. Load data ----
clinicalProject <- GetProject(syrfConnection$conProject, clinicalProjectId , raw=F)
clinicalAnnotations <- GetAnnotationsForProjectFull(syrfConnection$conStudy, syrfConnection$conInvestigator, syrfConnection$conProject, clinicalProject)
latestDatetime <- max(clinicalAnnotations$DateTimeCreated)
clinicalAnnotations <- CleanAnnotations(clinicalAnnotations)
# validateClinicalAnnotations <- ValidateLongAnnotations(clinicalAnnotations)
# if(nrow(validateClinicalAnnotations) != nrow(clinicalAnnotations)) print("Duplicated annotation exist!") else rm(validateClinicalAnnotations)

#----- 2. Calculation ----
# find the first investigator for each study. This is for the first run of the score, since we don't have reconcilation yet. 
validStudyInvestigatorIdStr <- clinicalAnnotations %>%
  filter(QuestionIdStr == unique(scoreRule$QuestionIdStr[scoreRule$ScoreType == "Efficacy"])) %>%
  arrange(DateTimeCreated) %>%
  group_by(
    StudyIdStr
  ) %>%
  summarise(
    InvestigatorIdStr = first(InvestigatorIdStr)
  )

clinicalAnnotations <- clinicalAnnotations %>%
  right_join(validStudyInvestigatorIdStr)

# Extract annotations at different level
rootAnnotations <- ExtractAnnotationsForTopQuestion(clinicalAnnotations, topColumnNames =  c("StudyId", "Study")) 
experimentAnnotations <- ExtractAnnotationsForTopQuestion(clinicalAnnotations, systemRootQuestions$experimentLabel, c("ExperimentIdStr", "ExperimentLabel"))
cohortAnnotations <- ExtractAnnotationsForTopQuestion( clinicalAnnotations, systemRootQuestions$cohortLabel, c("CohortIdStr", "CohortLabel"))
modelAnnotations <- ExtractAnnotationsForTopQuestion(clinicalAnnotations, systemRootQuestions$diseaseModelLabel, c("ModelIdStr", "ModelLabel"))
interventionAnnotations <- ExtractAnnotationsForTopQuestion(clinicalAnnotations, systemRootQuestions$treatmentLabel, c("InterventionIdStr", "InterventionLabel"))
outcomeAnnotations <- ExtractAnnotationsForTopQuestion(clinicalAnnotations, systemRootQuestions$outcomeLabel, c("OutcomeIdStr", "OutcomeLabel"))

# Spread annotations to columns
rootAnnotationsSpread <- SpreadAnnotations(rootAnnotations)
experimentAnnotationsSpread <- SpreadAnnotations(experimentAnnotations)
cohortAnnotationsSpread <- SpreadAnnotations(cohortAnnotations)
modelAnnotationsSpread <- SpreadAnnotations(modelAnnotations)
interventionAnnotationsSpread <- SpreadAnnotations(interventionAnnotations)
outcomeAnnotationsSpread <- SpreadAnnotations(outcomeAnnotations)

# identify interventionIdstr with valid intervention: has annotation 'efficacy outcome' and the answer is not "not drug of interest"
validEfficacyOutcomeAnnotationsSpread <- interventionAnnotationsSpread[which(!is.na(interventionAnnotationsSpread$`c5fec1a7-fe65-48d3-85ac-f4df93b796f0_efficacy outcome`) & interventionAnnotationsSpread$`c5fec1a7-fe65-48d3-85ac-f4df93b796f0_efficacy outcome` != "not drug of interest"), ]

validEfficacyOutcomeAnnotationsSpread$Interventions <- sapply(validEfficacyOutcomeAnnotationsSpread$`60a86326-8aa0-485d-9f8f-2e9d508b932d_What is the intervention?`, function(x){unique(strsplit(x, split=";")[[1]])})
validEfficacyOutcomeAnnotationsSpread <- unnest(validEfficacyOutcomeAnnotationsSpread, Interventions, names_repair = "universal")

index <- which(validEfficacyOutcomeAnnotationsSpread$Interventions != "other")

validInterventionIdStrs <- unique(validEfficacyOutcomeAnnotationsSpread$InterventionIdStr[index])

# Combine all annotations together to find all the relavent ids
validIdStrs <- cohortAnnotationsSpread

validIdStrs$InterventionIdStr <- sapply(validIdStrs$`a3f2e5bb-3ade-4830-bb66-b5550a3cc85b_Intervention`, function(x) strsplit(x, ";"))
validIdStrs <- unnest(validIdStrs, InterventionIdStr) %>%
  filter(
    InterventionIdStr %in% validInterventionIdStrs
  )

validIdStrs$ModelIdStr <- sapply(validIdStrs$`ecb550a5-ed95-473f-84bf-262c9faa7541_Disease Model`, function(x) strsplit(x, ";"))
validIdStrs <- unnest(validIdStrs,ModelIdStr)

validIdStrs$OutcomeIdStr <- sapply(validIdStrs$`12ecd826-85a4-499a-844c-bd35ea6624ad_Outcomes`, function(x) strsplit(x, ";"))
validIdStrs <- unnest(validIdStrs,OutcomeIdStr)

experimentAnnotationsSpread$CohortIdStr <- sapply(experimentAnnotationsSpread$`e7a84ba2-4ef2-4a14-83cb-7decf469d1a2_Cohorts`, function(x) strsplit(x, ";"))

experimentAnnotationsSpread <- unnest(experimentAnnotationsSpread,CohortIdStr)

validIdStrs <- validIdStrs %>% left_join(experimentAnnotationsSpread[,c("ExperimentIdStr","CohortIdStr")])

idsColumnNames <- c("CohortIdStr"  ,"StudyIdStr" , "InvestigatorIdStr" , "ExperimentIdStr"
                    ,"Investigator"
                    ,"InterventionIdStr" ,"ModelIdStr"  ,"OutcomeIdStr")

validIdStrs <- validIdStrs %>%
  select( idsColumnNames) 

scoreRelevantColumns <- c(idsColumnNames, allRelevantQuestionUnique)

# merge all valid annotation together into one DF with validIdStrs into allValidAnnotations
allValidAnnotations <- validIdStrs %>% 
  left_join(rootAnnotationsSpread) %>%
  left_join(experimentAnnotationsSpread) %>%
  left_join(cohortAnnotationsSpread) %>%
  left_join(modelAnnotationsSpread) %>%
  left_join(interventionAnnotationsSpread) %>%
  left_join(outcomeAnnotationsSpread) %>%
  select(scoreRelevantColumns)

allValidAnnotations[,"Disease"] <- allValidAnnotations[, diseaseQuestionIdStr]
allValidAnnotations[,"Intervention"] <- allValidAnnotations[, interventionIdStr]
allValidAnnotations[,"nAnimals"] <- allValidAnnotations[, nAnimalIdStr]

allValidAnnotations[,"efficacyScores"] <- calculate_categorical_scores_for_multipleQuestions(allValidAnnotations[, c(idsColumnNames, efficacyQuestionIdStr)], efficacyQuestionIdStr, scoreRule)
allValidAnnotations[,"safteyScores"] <- calculate_categorical_scores_for_multipleQuestions(allValidAnnotations[, c(idsColumnNames, safteyQuestionIdStr)], safteyQuestionIdStr, scoreRule)

allValidAnnotations[, paste0(qualityQuestionIdStr, "_score" ) ] <- calculate_categorical_scores_for_multipleQuestions(allValidAnnotations[, c(idsColumnNames, qualityQuestionIdStr )], qualityQuestionIdStr, scoreRule)

scoresfull <- allValidAnnotations[,c(idsColumnNames, "Disease", "Intervention", "nAnimals", "efficacyScores", "safteyScores", paste0(qualityQuestionIdStr, "_score" )) ]

# calculate the scores
scores <- scoresfull %>%
  group_by(
    StudyIdStr, CohortIdStr,Investigator, InvestigatorIdStr, ExperimentIdStr,  InterventionIdStr, Disease, nAnimals, Intervention
  ) %>%
  summarise_at(
    c("efficacyScores", "safteyScores", paste0(qualityQuestionIdStr, "_score" ))
    # setdiff(c("efficacyScores", "safteyScores", paste0(qualityQuestionIdStr, "_score" )), blindingIdStr)
    , max )

scores$Intervention <- sapply(scores$Intervention, function(x) {
  unique(strsplit(x, split = ";")[[1]])})
scores <- unnest(scores, Intervention) %>%
  filter(Intervention != "other")

# first group take out interventionIdStr as varaibles, but keep Intervention. If one intervention is used in one experiment in two different interventionIdStr within one cohort, we'll choose the higher score. This will prepare us for the next step for aggreating (sum up) the number of animals for an intervention within multiple cohorts. To achive that we use mutate to calculate it into nAnimal. For all the other quality scores, we take the highest in each group by
scores <- scores %>%
  group_by(
    StudyIdStr, CohortIdStr, Investigator, InvestigatorIdStr, ExperimentIdStr, Disease, Intervention, nAnimals
  ) %>%
  summarise_at(c("efficacyScores", "safteyScores", paste0(qualityQuestionIdStr, "_score" )), max) %>%
  group_by(
    StudyIdStr,Investigator, InvestigatorIdStr, ExperimentIdStr, Disease, Intervention
  ) %>%
  mutate(
    nAnimalPerIntervention = sum(as.numeric(nAnimals), na.rm = T)
  ) %>%
  group_by(
    StudyIdStr, Investigator, InvestigatorIdStr, ExperimentIdStr, Disease, Intervention, nAnimalPerIntervention
  ) %>%
  summarise_at(c("efficacyScores", "safteyScores", paste0(qualityQuestionIdStr, "_score" )),max) %>%
  mutate(
    studySizeScore = calculate_numberRange_score(nAnimalIdStr, nAnimalPerIntervention, scoreRule)
  )

scores$qualityScoreTotal <- rowSums(scores[,paste0(qualityQuestionIdStr, "_score" )])

qualityQuantile <- quantile(scores$qualityScoreTotal)

scoreRule1 <- data.frame(
  ScoreType = "Quality",
  Level = "Study",
  QuestionIdStr = "",
  Question = "qualityScore",
  Option = c(paste0(qualityQuantile[1],";",qualityQuantile[2]), paste0(qualityQuantile[2],";",qualityQuantile[3]), paste0(qualityQuantile[3],";",qualityQuantile[4]),paste0(qualityQuantile[4],";",qualityQuantile[5])),
  Score = c(1,2,3,4),
  AggregationMethod = "highest",
  QuestionUnique = "qualityScore"
)

scores <- scores %>%
  mutate(
    qualityScore = calculate_numberRange_score("qualityScore", qualityScoreTotal, scoreRule1)
  )

names(scores)[which(names(scores) == "Intervention")] <- "Drug"

publicationList <- scores[,c("StudyIdStr","nAnimalPerIntervention","Drug","Disease","efficacyScores","safteyScores","studySizeScore","qualityScore")] %>%
  left_join(experimentAnnotationsSpread[,c("StudyIdStr", "Title","Abstract","DOI","Year","Journal","Author", "56fd5eb1-e4eb-44ce-9db1-a88b674a43f7_Experiment/study type?","6cfbf4f6-5c80-40ac-8b1f-d11d863fe7b9_Clinical Trial Phase")]) %>%
  select("StudyIdStr",	"Disease",	"Drug",	"Title", "Abstract",	"DOI", "Year", "Author", "Journal",	"studyType" = "56fd5eb1-e4eb-44ce-9db1-a88b674a43f7_Experiment/study type?",	"phase" = "6cfbf4f6-5c80-40ac-8b1f-d11d863fe7b9_Clinical Trial Phase",	"nPatients" = "nAnimalPerIntervention", "efficacyScores", "safteyScores", "studySizeScore",	"qualityScore")

publicationList <- unique(publicationList)
googlesheets4::write_sheet(publicationList, googleSheetId, sheet = publicationListSheetName)

drugSummary <- scores %>%
  group_by(
  StudyIdStr,Investigator, InvestigatorIdStr, Disease, Drug
) %>%
  summarise_at(c("efficacyScores", "safteyScores", "studySizeScore","qualityScore"), max) %>%
  group_by(
    Drug
  )%>%
  summarise(
    nPD = sum(Disease == "PD"),
    nMND = sum(Disease == "MND"),
    nAD = sum(Disease == "AD"),
    nMS = sum(Disease == "MS"),
    nHD = sum(Disease == "HD"),
    nFTD = sum(Disease == "FTD"),
    # Diseases = list(Disease),
    nPublication = length(StudyIdStr),
    efficacyScore = mean(efficacyScores),
    safetyScore = mean(safteyScores),
    studySizeScore = mean(studySizeScore),
    qualityScore = mean(qualityScore),
    productScore = log10(nPublication+1)*median(efficacyScore)*median(safetyScore)*median(studySizeScore)*median(qualityScore),
    invivoSurvivalSMD = NA,
    nInvivoStudies = 0,
    nCellDeathSMD = NA,
    nInVitroStudies = 0
  )

googlesheets4::write_sheet(drugSummary, googleSheetId, sheet = drugSummarySheetName)


}
