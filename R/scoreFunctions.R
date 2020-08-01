create_file_folders <- function(inputFolder = "data/", outputFolder = "output/"){
  df <- list(
    InputFolder = inputFolder,
    OutputFolder = outputFolder 
  )
  dir.create(df$InputFolder, showWarnings = F)
  dir.create(df$OutputFolder, showWarnings = F)
  
  return(df)
}

calculate_categorical_scores_for_multipleQuestions <- function(scoreRelevantAnnotationsSpread, allRelevantQuestionUnique, scoreRule){
  for(ind in which(names(scoreRelevantAnnotationsSpread) %in% allRelevantQuestionUnique) ){
    df <- as.data.frame(scoreRelevantAnnotationsSpread)[,ind]

    scoreRelevantAnnotationsSpread[, paste0(names(scoreRelevantAnnotationsSpread)[ind], "_score" )] <-  sapply(df, calculate_multiple_categorical_score, questionUnique = names(scoreRelevantAnnotationsSpread)[ind], scoreRule = scoreRule)
  }
  index <- grep("_score", names(scoreRelevantAnnotationsSpread))
  return(scoreRelevantAnnotationsSpread[,index])
}

calculate_multiple_categorical_score <- function(questionUnique, answers, scoreRule){
  answers <- as.character(answers)
  answers <- strsplit(answers, ";")[[1]]
  results <- sapply(answers, calculate_categorical_score, questionUnique = questionUnique, scoreRule = scoreRule)

  re <- scoreRule %>%
    filter(
      QuestionUnique ==  questionUnique
    ) 
  aggregateMethod <- re[1,]$AggregationMethod
  
  if(aggregateMethod == "highest") {result <- max(results, na.rm = T)}
  else if(aggregateMethod == "add") {
    result <- sum(results, na.rm = T)}
  else {result <- max(results, na.rm = T)}
  
  return(result)
}

calculate_categorical_score <- function(questionUnique, answer, scoreRule){
  answer <- as.character(answer)
  result <- scoreRule %>%
    filter(
      QuestionUnique ==  questionUnique &  Option == answer
    )
  return(result$Score[1])
}

calculate_numberRange_score <- function(questionIdStr, answer, scoreRule){
  answer <- as.numeric(answer)
  rule <- scoreRule %>%
    filter(
      QuestionUnique ==  questionIdStr
    ) %>%
    mutate(
      sp = strsplit(unlist(Option),";")
    )
  rule$min <- sapply(rule$sp, function(x) as.numeric(first(x)))
  rule$max <- sapply(rule$sp, function(x) as.numeric(last(x)))
  
  result <- rule %>%
    filter(
      answer >= min & answer <= max
    ) 
  return(result$Score[1])
}