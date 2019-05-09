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

labkey.webdav.get <- function(baseUrl=NULL, folderPath, remoteFilePath, localFilePath, overwrite=TRUE, fileSet="@files")
{
    baseUrl=labkey.getBaseUrl(baseUrl);

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

    ret <- labkey.webdav.getByUrl(url, localFilePath, overwrite)
    if (ret == FALSE) {
      return(FALSE)
    }

    return(file.exists(localFilePath))
}

labkey.webdav.getByUrl <- function(url, localFilePath, overwrite=TRUE)
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

    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE){
        print(paste0("URL: ", url))
        response <- GET(url=url, write_disk(localFilePath, overwrite=overwrite), config=options, verbose(data_in=TRUE, info=TRUE, ssl=TRUE))
    } else {
        response <- GET(url=url, write_disk(localFilePath, overwrite=overwrite), config=options)
    }

    processResponse(response)
}

labkey.webdav.put <- function(localFile, baseUrl=NULL, folderPath, remoteFilePath, fileSet="@files")
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
    baseUrl=labkey.getBaseUrl(baseUrl);

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
    baseUrl=labkey.getBaseUrl(baseUrl);

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
    baseUrl=labkey.getBaseUrl(baseUrl);

    url <- labkey.webdav.validateAndBuildRemoteUrl(baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=remoteFilePath)
    url <- paste0(url, "?method=JSON")
    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
        print(paste0("URL: ", url))
    }

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
    baseUrl=labkey.getBaseUrl(baseUrl);

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
    baseUrl=labkey.getBaseUrl(baseUrl);

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

labkey.webdav.downloadFolder <- function(localDir, baseUrl=NULL, folderPath, remoteFilePath, overwrite=TRUE, fileSet="@files")
{
    if (!file.exists(localDir)) {
        stop(paste0("Download folder does not exist: ", localDir))
    }

    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(remoteFilePath)){
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, and remoteFilePath"))
    }

    # Note: this should use unencoded values to match the ID in JSON
    folderPath <- normalizeSlash(folderPath)
    prefix <- paste0("/_webdav", folderPath, fileSet, "/")  
    
    files <- labkey.webdav.listDir(baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=remoteFilePath)
    for (file in files[["files"]]) {
      relativePath <- sub(prefix, "", file[["id"]])
      localPath <- file.path(localDir, relativePath)
      if (file[["isdirectory"]]) {
          if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
              print(paste0("Downloading folder: ", relativePath))
              print(paste0("to: ", localPath))
          }

          if (!file.exists(localPath)){
              dir.create(localPath, recursive=TRUE)
          }

          labkey.webdav.downloadFolder(localDir=localDir, baseUrl=baseUrl, folderPath=folderPath, fileSet=fileSet, remoteFilePath=relativePath)
      } else {
          url <- paste0(baseUrl, file[["href"]])

          if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
              print(paste0("Downloading file: ", relativePath))
              print(paste0("to: ", localPath))
          }
          labkey.webdav.getByUrl(url, localPath, overwrite)
      }
    }

    return(TRUE)
}