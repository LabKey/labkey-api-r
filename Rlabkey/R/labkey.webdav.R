##
#  Copyright (c) 2019 LabKey Corporation
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
##

labkey.webdav.get <- function(baseUrl=NULL, folderPath, remoteFilePath, localFilePath, overwrite=TRUE, fileSet="@files", showProgressBar=FALSE)
{
    baseUrl <- labkey.getBaseUrl(baseUrl);

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(remoteFilePath) || missing(localFilePath)){
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, remoteFilePath, and localFilePath"));
    }

    if (labkey.webdav.isDirectory(baseUrl = baseUrl, folderPath = folderPath, remoteFilePath = remoteFilePath, fileSet = fileSet, haltOnError = T)){
      stop('The requested file is a directory.  Please see labkey.webdav.downloadFolder()')  
    }
    
    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath);
    remoteFilePath <- encodeRemotePath(remoteFilePath)

    url <- paste(baseUrl, "_webdav", folderPath, fileSet, "/", remoteFilePath, sep="");

    ret <- labkey.webdav.getByUrl(url, localFilePath, overwrite, showProgressBar = showProgressBar)
    if (!is.null(ret) && !is.na(ret) && ret == FALSE) {
      return(FALSE)
    }

    return(file.exists(localFilePath))
}

labkey.webdav.getByUrl <- function(url, localFilePath, overwrite=TRUE, showProgressBar=FALSE)
{
    # dont bother querying if this file already exists, since we wont overwrite it
    if (!overwrite & file.exists(localFilePath)) {
        return(FALSE)
    }
  
    if (dir.exists(localFilePath)) {
      stop(paste0("The local filepath exists and is a directory: ", localFilePath))
    }
  
    localDownloadDir <- dirname(localFilePath)
    if (!file.exists(localDownloadDir)) {
        dir.create(localDownloadDir, recursive=TRUE)
    }

    options <- labkey.getRequestOptions(method="GET")

    progressBar <- NULL
    if (showProgressBar) {
      progressBar <- httr::progress()
    }

    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE){
        print(paste0("URL: ", url))
        response <- GET(url=url, write_disk(localFilePath, overwrite=overwrite), config=options, verbose(data_in=TRUE, info=TRUE, ssl=TRUE), progressBar)
    } else {
        response <- GET(url=url, write_disk(localFilePath, overwrite=overwrite), config=options, progressBar)
    }

    processResponse(response)
}

labkey.webdav.put <- function(localFile, baseUrl=NULL, folderPath, remoteFilePath, fileSet="@files", description=NULL)
{
    if (missing(localFile)) {
        stop (paste("A value must be specified for localFile"))
    }

    if (!file.exists(localFile)){
        stop (paste0("File does not exist: ", localFile));
    }

    url <- labkey.webdav.validateAndBuildRemoteUrl(baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=remoteFilePath)
    options <- labkey.getRequestOptions(method="POST")

    pbody <- upload_file(localFile)

    if (!is.null(description)) {
        url <- paste0(url, "?description=", URLencode(description))
    }

    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
        print(paste0("URL: ", url))
        response <- PUT(url=url, config=options, body=pbody, verbose(data_in=TRUE, info=TRUE, ssl=TRUE))
    } else {
        response <- PUT(url=url, config=options, body=pbody)
    }

    processResponse(response, responseType="text/plain; charset=utf-8")

    return(TRUE)
}

labkey.webdav.mkDir <- function(baseUrl=NULL, folderPath, remoteFilePath, fileSet="@files")
{
    url <- labkey.webdav.validateAndBuildRemoteUrl(baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=remoteFilePath)

    options <- labkey.getRequestOptions(method="POST")

    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
        print(paste0("URL: ", url))
        response <- VERB("MKCOL", url=url, config=options, verbose(data_in=TRUE, info=TRUE, ssl=TRUE))
    } else {
        response <- VERB("MKCOL", url=url, config=options)
    }

    processResponse(response, responseType="text/plain; charset=utf-8")

    return(TRUE)
}

labkey.webdav.validateAndBuildRemoteUrl <- function(baseUrl=NULL, folderPath, remoteFilePath, fileSet="@files")
{
    baseUrl <- labkey.getBaseUrl(baseUrl);

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(fileSet) || missing(remoteFilePath)){
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, and remoteFilePath"));
    }
    
    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath);
    remoteFilePath <- encodeRemotePath(remoteFilePath)

    return(paste(baseUrl, "_webdav", folderPath, fileSet, "/", remoteFilePath, sep=""))
}

encodeRemotePath <- function(path, splitSlash = TRUE) {
    if (splitSlash) {
        path <- strsplit(path, "/")[[1]]
    }
    return(paste0(sapply(path, URLencode, reserved = T), collapse = '/'))
}

labkey.webdav.pathExists <- function(baseUrl=NULL, folderPath, remoteFilePath, fileSet="@files")
{
    baseUrl <- labkey.getBaseUrl(baseUrl);

    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(remoteFilePath)) {
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, and remoteFilePath"));
    }
    
    ret <- labkey.webdav.listDir(baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=remoteFilePath, haltOnError=F)
    
    return(is.null(ret$exception))
}

labkey.webdav.isDirectory <- function(baseUrl=NULL, folderPath, remoteFilePath, fileSet="@files", haltOnError = TRUE) {
  json <- labkey.webdav.listDir(baseUrl = baseUrl, folderPath = folderPath, remoteFilePath = remoteFilePath, fileSet = fileSet, haltOnError = haltOnError)
  
  return(!is.null(json[['fileCount']]))
}

labkey.webdav.listDir <- function(baseUrl=NULL, folderPath, remoteFilePath, fileSet="@files", haltOnError = TRUE)
{
    baseUrl <- labkey.getBaseUrl(baseUrl);

    url <- labkey.webdav.validateAndBuildRemoteUrl(baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=remoteFilePath)
    url <- paste0(url, "?method=JSON")
    logMessage(paste0("URL: ", url))

    content <- labkey.post(url, pbody="", responseType="text/plain; charset=utf-8", haltOnError = haltOnError)

    # The intent of this is to mask some of the properties only intended for rendering the file browser UI (like icon)
    ret <- fromJSON(content, simplifyVector=FALSE, simplifyDataFrame=FALSE)
    colNames <- c("id", "href", "text", "creationdate", "createdby", "lastmodified", "contentlength", "size", "isdirectory")
    ret[["files"]] <- lapply(ret[["files"]], function(l){
        idx <- match("collection", names(l))
        if (!is.na(idx)){
            names(l)[idx] <- "isdirectory"
        } else {
            l$isdirectory <- FALSE
        }

        l <- l[colNames]
        names(l) <- colNames
        return(l)
    })

    return(ret)
}

labkey.webdav.delete <- function(baseUrl=NULL, folderPath, remoteFilePath, fileSet="@files")
{
    baseUrl <- labkey.getBaseUrl(baseUrl);

    url <- labkey.webdav.validateAndBuildRemoteUrl(baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=remoteFilePath)
    url <- paste0(url, "?method=DELETE")
    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
        print(paste0("URL: ", url))
    }

    labkey.post(url, pbody="", responseType="text/plain; charset=utf-8")
    
    return(T)
}

labkey.webdav.mkDirs <- function(baseUrl=NULL, folderPath, remoteFilePath, fileSet="@files")
{
    baseUrl <- labkey.getBaseUrl(baseUrl);

    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(remoteFilePath)){
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, and remoteFilePath"))
    }

    remoteFilePaths <- strsplit(remoteFilePath, "/")[[1]]
    toCreate <- ""
    for (folderName in remoteFilePaths) {
        toCreate <- paste0(toCreate, folderName, "/")
        if (!labkey.webdav.pathExists(baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=toCreate)) {
            if (!labkey.webdav.mkDir(baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=toCreate)){
                stop(paste0("Failed to create folder: ", toCreate))
            }
        }
    }

    return(TRUE)
}

labkey.webdav.downloadFolder <- function(localBaseDir, baseUrl=NULL, folderPath, remoteFilePath, overwriteFiles=TRUE, mergeFolders=TRUE, fileSet="@files", showProgressBar=FALSE) {
  if (missing(localBaseDir) || missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(remoteFilePath)){
    stop (paste("A value must be specified for each of localBaseDir, baseUrl, folderPath, fileSet, and remoteFilePath"))
  }
  
  if (file.exists(localBaseDir) && !dir.exists(localBaseDir)) {
    stop(paste0("Download folder exists, but is not a directory: ", localBaseDir))
  }
  
  # Download remote directory directly into this newly created folder:
  if (!dir.exists(localBaseDir)) {
    stop(paste0("Download folder does not exist: ", localBaseDir))
  }
  
  # always download into a subfolder with the basename of the remote directory
  remoteFilePath <- normalizeSlash(remoteFilePath, leading = F)
  subfolder <- basename(remoteFilePath)
  if (subfolder != ''){
    localBaseDir <- normalizeFolder(localBaseDir)
    localBaseDir <- file.path(localBaseDir, subfolder)
    logMessage(paste0('target local folder: ', localBaseDir))
  }

  if (!labkey.webdav.isDirectory(baseUrl = baseUrl, folderPath = folderPath, remoteFilePath = remoteFilePath, fileSet = fileSet, haltOnError = T)){
    stop('The requested file is not a directory.')  
  }
  
  if (!prepareDirectory(localPath = localBaseDir, overwriteFiles = overwriteFiles, mergeFolders = mergeFolders)){
    return(F)
  }
  
  labkey.webdav.doDownloadFolder(localDir = localBaseDir, baseUrl = baseUrl, folderPath = folderPath, remoteFilePath = remoteFilePath, overwriteFiles = overwriteFiles, mergeFolders = mergeFolders, fileSet = fileSet, showProgressBar = showProgressBar)
}

normalizeFolder <- function(localDir){
  # remove trailing or double slash
  localDir <- gsub("[\\]", "/", localDir)
  localDir <- gsub("[/]+", "/", localDir)
  if (substr(localDir, nchar(localDir), nchar(localDir))=="/") {
    localDir <- substr(localDir,1, nchar(localDir)-1)
  }
  
  return(localDir)
}

logMessage <- function(msg) {
  if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
    print(msg)  
  }
}

labkey.webdav.doDownloadFolder <- function(localDir, baseUrl=NULL, folderPath, remoteFilePath, overwriteFiles=TRUE, mergeFolders=TRUE, fileSet="@files", showProgressBar=FALSE)
{
    baseUrl <- normalizeSlash(baseUrl, leading = F, trailing = F)
    folderPath <- encodeFolderPath(folderPath);
    fileSet <- normalizeSlash(fileSet, leading = F, trailing = F)
    remoteFilePath <- normalizeSlash(encodeRemotePath(remoteFilePath), leading = F)
    localDir <- normalizeFolder(localDir)

    # Issue 51606: match file path prefix to the href values returned in listDir, which are URL encoded paths
    prefix <- paste0("/_webdav", folderPath, encodeRemotePath(fileSet), '/')

    files <- labkey.webdav.listDir(baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=remoteFilePath)
    for (file in files[["files"]]) {
      # Issue 53043: the files list href includes the server contextPath, so trim that if available
      fileHref <- file[["href"]]
      prefixIndex <- regexpr(prefix, fileHref)
      if (prefixIndex > -1) {
        fileHref <- substr(fileHref, prefixIndex, nchar(fileHref))
      }

      relativeToRemoteRoot <- sub(prefix, "", fileHref)
      relativeToDownloadStart <- sub(paste0(prefix, remoteFilePath), "", fileHref)

      localPath <- file.path(localDir, relativeToDownloadStart)
      if (file[["isdirectory"]]) {
        logMessage(paste0("Downloading folder: ", relativeToRemoteRoot))
        logMessage(paste0("to: ", localPath))

        # File exists, but is not directory:
        if (file.exists(localPath) && !dir.exists(localPath)) {
          if (overwriteFiles) {
            unlink((localPath))
          } else {
            stop(paste0('Target of folder download already exists, but is a file, not a folder: ', localPath))
          }  
        }
      
        # Handle potential merges:
        if (!prepareDirectory(localPath, overwriteFiles, mergeFolders)) {
          next
        }
        
        labkey.webdav.doDownloadFolder(localDir=localPath, baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=relativeToRemoteRoot, overwriteFiles=overwriteFiles, mergeFolders=mergeFolders, showProgressBar=showProgressBar)
      } else {
          url <- paste0(baseUrl, trimLeadingPath(fileHref))

          logMessage(paste0("Downloading file: ", relativeToRemoteRoot))
          logMessage(paste0("to: ", localPath))

          labkey.webdav.getByUrl(url, localPath, overwriteFiles, showProgressBar=showProgressBar)
      }
    }

    return(TRUE)
}

trimLeadingPath <- function(url){
  pos <- regexpr('/_webdav', tolower(url))
  if (pos == -1) {
    return(url)
  }
  
  return(substr(url, pos, nchar(url)))
}

prepareDirectory <- function(localPath, overwriteFiles, mergeFolders) {
  if (dir.exists(localPath)) {
    logMessage(paste0('existing folder found: ', localPath))  
    
    if (!mergeFolders && overwriteFiles) {
      logMessage('deleting existing folder')
      unlink(localPath, recursive = TRUE)
    }
    else if (!mergeFolders && !overwriteFiles) {
      logMessage('skipping existing folder')
      return(F)
    }
    else if (mergeFolders) {
      logMessage('existing folder will be left alone and contents downloaded')
    }
  } else {
    dir.create(localPath, recursive=TRUE)
  }
  
  return(TRUE)
}