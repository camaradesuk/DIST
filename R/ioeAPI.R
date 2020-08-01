#---------- File and folder handlers -------------------------------------------------
# Functions wrapping around "AzureStor" functions for handling file uploading to blob container  
# Created by Jing Liao
# Created on Dec 28 2018

# Files and Folders 

require(AzureStor)
require(rlang)
require(httr)
require(RCurl)
require(magrittr)

CreateFileNamesForIOEAPIProjectId <- function(projectId, fileFolders){
  list( 
    DataFileName = paste0(fileFolders$InputFolder, projectId , ".csv"),
    LabelFileName = paste0(fileFolders$InputFolder, projectId , "Labels.csv"),
    VectorFileName = paste0(fileFolders$OutputFolder, projectId , "Vectors.csv"),
    ResultsFileName = paste0(fileFolders$OutputFolder , projectId , "Results.csv")
  )
}

CreateFileNamesForIOEAPI <- function(dataFileName,labelFileName,vectorFileName, resultsFileName){
  list( 
    DataFileName = dataFileName,
    LabelFileName = labelFileName,
    VectorFileName = vectorFileName,
    ResultsFileName = resultsFileName
  )
}

ReformatFile<-function(filename){
  filedata <- read.csv(filename , stringsAsFactors = F  )
  # filedata <- as.data.frame(sapply(filedata, as.character), stringsAsFactors = F )
  filedata$REVIEW_ID <- as.character(filedata$REVIEW_ID)
  filedata$ITEM_ID <- as.character(filedata$ITEM_ID)
  write.csv(filedata, file=filename,row.names = F, quote = T, na = "")
}

SaveTo1234 <- function(filenames, fileFolders){
  filenames1234 <- CreateFileNamesForIOEAPIProjectId("1234", fileFolders)
  
  filedata <- read.csv(filenames$DataFileName)
  write.csv( filedata, file=filenames1234$DataFileName,row.names = F, quote = T, na = "")
  
  filedata <- read.csv(filenames$LabelFileName)
  write.csv( filedata, file=filenames1234$LabelFileName,row.names = F, quote = T, na = "")
  
  return(filenames1234)
}

ConvertResultFrom1234 <- function(filenames, filenames1234){
  # filedata <- read.csv(filenames1234$ResultsFileName)
  # write.csv(filedata, file=filenames$ResultsFileName, row.names = F, quote = T), na = ""
  file.copy(filenames1234$ResultsFileName, filenames$ResultsFileName, overwrite = T)
  # file.rename(filenames1234$ResultsFileName, filenames$ResultsFileName)
}

# ---------- Blob uploading Wrappers -------------------------------------------------
# Functions wrapping around "AzureStor" functions for handling file uploading to blob container  
# Created by Jing Liao
# Created on Dec 28 2018
require(AzureStor)
require(magrittr)
require(rlang)
require(httr)
source("R/importedFunctions/blob_client_funcs.R")
source("R/importedFunctions/storage_utils.R")

JTBlobContainer <- function()
{ 
  jtBlob <- JTBlob()
  blob_endpoint(jtBlob$endpoint, sas = jtBlob$sas) %>% blob_container(jtBlob$container)
}

UploadFileToContainer <- function(container, srcFilename, destFilename){
  print(paste0("Uploading file ", srcFilename, " as ", destFilename))
  upload_blob(container, srcFilename, destFilename)
  if(destFilename %in% (list_blobs(container)$Name)) print("--- Succeed! ---") else print("Failed!")
}

UploadFilesToContainer <- function(container, srcFilenames, destFilenames){
  for(ind in seq_along(srcFilenames)){
    filename <- as.character(srcFilenames[ind])
    destFilename <- as.character(destFilenames[ind])
    UploadFileToContainer(container, filename, destFilename)
  }
}

# ---------- Vecotrisation API Wrappers -------------------------------------------------
# Functions handeling hit the API for vectorisation. (POST)
# Created by Jing Liao
# Created on Dec 29 2018
require(RCurl)
require(rjson)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

requestFailed = function(headers) {
  return (headers["status"] >= 400)
}

printHttpError = function(headers, result) {
  print(paste("The request failed with status code:", headers["status"], sep=" "))
  
  # Print the headers - they include the requert ID and the timestamp, which are useful for debugging the failure
  print(headers)
  print(fromJSON(result))
}

processResults = function(result) {
  for (outputName in names(result$Results))
  {
    result_blob_location = result$Results[[outputName]]
    sas_token = result_blob_location$SasBlobToken
    base_url = result_blob_location$BaseLocation
    relative_url = result_blob_location$RelativeLocation
    
    print(paste("The result for", outputName, "is available at the following Azure Storage location:", sep=" "))
    print(paste("BaseLocation: ", base_url, sep=""))
    print(paste("RelativeLocation: ", relative_url, sep=""))
    print(paste("SasBlobToken: ", sas_token, sep=""))
  }
}

invokeBatchExecutionService <- function(url, apiKey, filenames, bodyContentJson) {
  authz_hdr = paste("Bearer", apiKey, sep=" ")
  
  print(bodyContentJson)
  
  body = enc2utf8(bodyContentJson)
  h = basicTextGatherer()
  hdr = basicHeaderGatherer()

  # submit the job
  print("Submitting the job...")
        
  curlPerform(url = paste(url, "?api-version=2.0", sep=""),
              httpheader = c("Content-Type" = "application/json", "Authorization" = authz_hdr),
              postfields = body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = FALSE
  )
  
  headers = hdr$value()
  result = h$value()
  if (requestFailed(headers)) {
    printHttpError(headers, result)
    return()
  }
  
  job_id = substring(result, 2,nchar(result)-1) # Removes the enclosing double-quotes
  
  print(paste("Job ID:", job_id, sep=" "))
  
  # start the job
  print("Starting the job...")
  h$reset()
  hdr$reset()
  curlPerform(url = paste(url, "/", job_id, "/start?api-version=2.0", sep=""),
              httpheader = c("Authorization" = authz_hdr),
              postfields = "",
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = FALSE
  )
  
  headers = hdr$value()
  result = h$value()
  if (requestFailed(headers)) {
    printHttpError(headers, result)
    return()
  }
  
  url2 = paste(url, "/", job_id, "?api-version=2.0", sep="")
  
  while (TRUE) {
    print("Checking the job status...")
    h$reset()
    hdr$reset()
    curlPerform(url = url2,
                httpheader = c("Authorization" = authz_hdr),
                writefunction = h$update,
                headerfunction = hdr$update,
                verbose = FALSE
    )
    
    headers = hdr$value()
    result = h$value()
    if (requestFailed(headers)) {
      printHttpError(headers, result)
      return()
    }
    
    result = fromJSON(result)
    status = result$StatusCode
    if (status == 0 || status == "NotStarted") {
      print(paste("Job", job_id, "not yet started...", sep=" "))
    }
    else if (status == 1 || status == "Running") {
      print(paste("Job", job_id, "running...", sep=" "))
    }
    else if (status == 2 || status == "Failed") {
      print(paste("Job", job_id, "failed...", sep=" "))
      print(paste("Error details:",  result$Details, sep=" "))
      break
    }
    else if (status == 3 || status == "Cancelled") {
      print(paste("Job", job_id, "cancelled...", sep=" "))
      break
    }
    else if (status == 4 || status == "Finished") {
      print(paste("Job", job_id, "finished...", sep=" "))
      
      processResults(result)
      break
    }
    Sys.sleep(60) # Wait 
  }
}

Vectorization <- function(jtAPI, container, filenames)
{
  print("--- Vectorization started ---")
  
  bodyContentJson <- paste0('{ "GlobalParameters": {"DataFile" : "', container$name, "/",basename(filenames$DataFileName)
                        ,'", "LabelsFile" : "',container$name, "/",basename(filenames$LabelFileName)
                        ,'","VectorsFile" : "' , container$name, "/",basename(filenames$VectorFileName),'"}}')
  
  invokeBatchExecutionService(jtAPI$VectoriseBaseUrl, jtAPI$VectoriseAPIKey,  filenames, bodyContentJson )
  
  if(basename(filenames$VectorFileName) %in% (list_blobs(container)$Name)) print("--- Vectorisation succeeded! ---") 
  else { print("--- Vectorisation Failed! ---")
   stop()}
}

# ---------- score -----
Score <- function(jtAPI, container, filenames)
{
  print("--- Scoring started ---")
  bodyContentJson <- paste0('{ "GlobalParameters": {"DataFile" : "', container$name, "/",basename(filenames$DataFileName)
                            ,'", "LabelsFile" : "',container$name, "/",basename(filenames$LabelFileName)
                            ,'","VectorsFile" : "' , container$name, "/",basename(filenames$VectorFileName)
                            ,'","ResultsFile" : "' , container$name, "/",basename(filenames$ResultsFileName)
                            ,'"}}')
  
  invokeBatchExecutionService(jtAPI$ScoreBaseUrl, jtAPI$ScoreApiKey,filenames, bodyContentJson )
  
  if(basename(filenames$ResultsFileName) %in% (list_blobs(container)$Name)) print("--- Score succeeded! ---") 
  else { print("--- Score Failed! ---")
    stop()}
}

# ---------- Warpper
TrainCollection <- function(srcfilenames, projectId, forceVectorization = F ){
  lapply(srcfilenames[c("DataFileName","LabelFileName")], ReformatFile)
  
  projectIdFilenames <- CreateFileNamesForIOEAPIProjectId(projectId
                                                          , fileFolders = create_file_folders(inputFolder = "", outputFolder = ""))
  
  syrfContainer <- JTBlobContainer()
  jtAPI <- JTAPI()
  
  HasDifferentDataFile <- function(container, destFilename, srcFilename, tempFilename = "temp.csv"){
    
    exsitingFilenames <- list_blobs(container)$Name
    newItemId <- list()
    vanishId <- list()
    
    if(!(destFilename %in% basename(exsitingFilenames))) return(TRUE)
    
    download_blob(container, destFilename, tempFilename, overwrite = T)
    tempData <- read.csv(tempFilename)
    newData <- read.csv(srcFilename)
    newItemId <- setdiff(newData$ITEM_ID, tempData$ITEM_ID)
    vanishId <- setdiff(tempData$ITEM_ID, newData$ITEM_ID)
    
    if(file.exists(tempFilename)) file.remove(tempFilename)
    return(length(newItemId) > 0 | length(vanishId) >0 )
  }
  
  if(forceVectorization)  doVectorization <- T else doVectorization <- HasDifferentDataFile(syrfContainer, projectIdFilenames$DataFileName, srcfilenames$DataFileName)
    
    if(doVectorization){
      UploadFilesToContainer(syrfContainer, srcfilenames[c("DataFileName","LabelFileName")]
                             , projectIdFilenames[c("DataFileName","LabelFileName")])
      
      Vectorization(jtAPI, syrfContainer, projectIdFilenames)
    }
    else {
      UploadFilesToContainer(syrfContainer, srcfilenames[c("LabelFileName")]
                             , projectIdFilenames[c("LabelFileName")])
    }
  Score(jtAPI, syrfContainer, projectIdFilenames)
  
  # download_blob(syrfContainer, projectIdFilenames$VectorFileName, filenames$VectorFileName, overwrite = T)
  download_blob(syrfContainer, projectIdFilenames$ResultsFileName, srcfilenames$ResultsFileName, overwrite = T)
}

TrainCollection1234 <- function(dataFileName, labelFilename, resultFilename){
  fileFolders1234 <- CreateFileFolders("temp/","temp/")

  filenames <- list( 
    DataFileName = dataFileName,
    LabelFileName = labelFilename,
    ResultsFileName = resultFilename
  )
  
  filenames1234 <- SaveTo1234(filenames, fileFolders1234)
  
  TrainCollection(filenames1234)
  
  ConvertResultFrom1234(filenames, filenames1234)

  sapply(CreateFileNamesForIOEAPIProjectId("1234", fileFolders1234), file.remove)
    
  read.csv(filenames$ResultsFileName, header = FALSE,col.names = c("score", "ITEM_ID","REVIEW_ID") )
}

TrainCollectionWithProjectId <- function(projectId, fileFolders){
  filenames <- CreateFileNamesForIOEAPIProjectId(projectId, fileFolders)
  
  TrainCollection(filenames, projectId)
  
  read.csv(filenames$ResultsFileName, header = FALSE,col.names = c("score", "ITEM_ID","REVIEW_ID") )
}