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

labkey.webdav.get <- function(baseUrl=NULL, folderPath, fileSet="@files", remoteFilePath, localFilePath, overwrite=TRUE)
{
    baseUrl=labkey.getBaseUrl(baseUrl);

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(remoteFilePath) || missing(localFilePath)){
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, remoteFilePath, and localFilePath"));
    }
    
    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath);

    url <- paste(baseUrl, "_webdav", folderPath, fileSet, "/", remoteFilePath, sep="");

    labkey.webdav.getByUrl(url, localFilePath, overwrite)

    return(file.exists(localFilePath))
}

labkey.webdav.getByUrl <- function(url, localFilePath, overwrite=TRUE)
{
    # dont bother querying if this file already exists, since we wont overwrite it
    if (!overwrite & file.exists(localFilePath)) {
        return()
    }

    localDownloadDir <- dirname(localFilePath)
    if (!file.exists(localDownloadDir)) {
        dir.create(localDownloadDir, recursive = TRUE)
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

labkey.webdav.put <- function(localFile, baseUrl=NULL, folderPath, fileSet="@files", remoteFilePath)
{
    if (missing(localFile)) {
        stop (paste("A value must be specified for localFile"))
    }

    if (!file.exists(localFile)){
        stop (paste0("File does not exist: ", localFile));
    }

    url <- labkey.webdav._validateRemoteUrl(baseUrl = baseUrl, folderPath = folderPath, fileSet = fileSet, remoteFilePath = remoteFilePath)
    options <- labkey.getRequestOptions(method="POST")

    pbody <- upload_file(localFile)

    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
        print(paste0("URL: ", url))
        response <- PUT(url=url, config=options, body=pbody, verbose(data_in=TRUE, info=TRUE, ssl=TRUE))
    } else {
        response <- PUT(url=url, config=options, body=pbody)
    }

    processResponse(response, responseType = "text/plain; charset=utf-8")

    return(TRUE)
}

labkey.webdav.mkDir <- function(baseUrl=NULL, folderPath, fileSet="@files", remoteFilePath)
{
    url <- labkey.webdav._validateRemoteUrl(baseUrl = baseUrl, folderPath = folderPath, fileSet = fileSet, remoteFilePath = remoteFilePath)

    options <- labkey.getRequestOptions(method="POST")

    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
        print(paste0("URL: ", url))
        response <- VERB("MKCOL", url=url, config=options, verbose(data_in=TRUE, info=TRUE, ssl=TRUE))
    } else {
        response <- VERB("MKCOL", url=url, config=options)
    }

    processResponse(response, responseType = "text/plain; charset=utf-8")

    return(TRUE)
}

labkey.webdav._validateRemoteUrl <- function(baseUrl=NULL, folderPath, fileSet="@files", remoteFilePath)
{
    baseUrl=labkey.getBaseUrl(baseUrl);

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(fileSet) || missing(remoteFilePath)){
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, and remoteFilePath"));
    }
    
    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath);

    return(paste(baseUrl, "_webdav", folderPath, fileSet, "/", remoteFilePath, sep=""))
}

labkey.webdav.pathExists <- function(baseUrl=NULL, folderPath, fileSet="@files", remoteFilePath)
{
    baseUrl=labkey.getBaseUrl(baseUrl);

    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(remoteFilePath)) {
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, and remoteFilePath"));
    }
    
    ret <- labkey.webdav.listDir(baseUrl, folderPath, fileSet, remoteFilePath)
    return(is.null(ret$exception))
}

labkey.webdav.listDir <- function(baseUrl=NULL, folderPath, fileSet="@files", remoteFilePath)
{
    baseUrl=labkey.getBaseUrl(baseUrl);

    url <- labkey.webdav._validateRemoteUrl(baseUrl = baseUrl, folderPath = folderPath, fileSet = fileSet, remoteFilePath = remoteFilePath)
    url <- paste0(url, "?method=JSON")
    content <- labkey.post(url, pbody = "", responseType = "text/plain; charset=utf-8")

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

labkey.webdav.delete <- function(baseUrl=NULL, folderPath, fileSet="@files", remoteFilePath)
{
    baseUrl=labkey.getBaseUrl(baseUrl);

    url <- labkey.webdav._validateRemoteUrl(baseUrl = baseUrl, folderPath = folderPath, fileSet = fileSet, remoteFilePath = remoteFilePath)
    url <- paste0(url, "?method=DELETE")

    labkey.post(url, pbody = "", responseType = "text/plain; charset=utf-8")
}

labkey.webdav.mkDirs <- function(baseUrl=NULL, folderPath, fileSet="@files", remoteFilePath)
{
    baseUrl=labkey.getBaseUrl(baseUrl);

    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(remoteFilePath)){
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, and remoteFilePath"))
    }

    remoteFilePaths <- strsplit(remoteFilePath, "/")[[1]]
    toCreate <- ""
    for (folderName in remoteFilePaths) {
        toCreate <- paste0(toCreate, folderName, "/")
        if (!labkey.webdav.pathExists(baseUrl, folderPath, fileSet, toCreate)) {
            if (!labkey.webdav.mkDir(baseUrl, folderPath, fileSet, toCreate)){
                stop(paste0("Failed to create folder: ", toCreate))
            }
        }
    }

    return(TRUE)
}

labkey.webdav.downloadFolder <- function(localDir, baseUrl=NULL, folderPath, fileSet="@files", remoteFilePath, overwrite = TRUE)
{
    if (!file.exists(localDir)) {
        stop(paste0("Download folder does not exist: ", localDir))
    }

    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(remoteFilePath)){
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, and remoteFilePath"))
    }

    folderPath <- encodeFolderPath(folderPath)
    prefix <- paste0("/_webdav", folderPath, fileSet, "/")
    files <- labkey.webdav.listDir(baseUrl, folderPath, fileSet, remoteFilePath)
    for (file in files[["files"]]) {
        relativePath <- sub(prefix, "", file[["id"]])
        localPath <- file.path(localDir, relativePath)
        if (file[["isdirectory"]]) {
            if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
                print(paste0("Downloading folder: ", relativePath))
                print(paste0("to: ", localPath))
            }

            if (!file.exists(localPath)){
                dir.create(localPath, recursive = TRUE)
            }

            labkey.webdav.downloadFolder(localDir, baseUrl, folderPath, fileSet, relativePath)
        } else {
            url <- paste0(baseUrl, file[["id"]])

            if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
                print(paste0("Downloading file: ", relativePath))
                print(paste0("to: ", localPath))
            }
            labkey.webdav.getByUrl(url, localPath, overwrite)
        }
    }

    return(TRUE)
}