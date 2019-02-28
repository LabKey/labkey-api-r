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
    if(missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(fileSet) || missing(remoteFilePath) || missing(localFilePath))
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, remoteFilePath, and localFilePath"));

    # dont bother querying if this file already exists, since we wont overwrite it
    if (!overwrite & file.exists(localFilePath)) {
      return()
    }
    
    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath);

    url <- paste(baseUrl, "_webdav", folderPath, fileSet, "/", remoteFilePath, sep="");

    options <- labkey.getRequestOptions(method="GET")
    
    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE){
      print(paste0("URL: ", url))
      response <- GET(url=url, write_disk(localFilePath, overwrite=overwrite), config=options, verbose(data_in=TRUE, info=TRUE, ssl=TRUE))
    } else {
      response <- GET(url=url, write_disk(localFilePath, overwrite=overwrite), config=options)
    }
    
    processResponse(response)
}


labkey.webdav.put <- function(baseUrl=NULL, localFile, folderPath, fileSet="@files", remoteFilePath, overwriteRemote=TRUE, encoding = 'UTF-8')
{
    baseUrl=labkey.getBaseUrl(baseUrl);
    
    ## check required parameters
    if(missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(fileSet) || missing(remoteFilePath) || missing(localFile))
      stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, localFile, and remoteFilePath"));
    
    if (!file.exists(localFile)){
      stop (paste("File does not exist: ", localFile));
    }
    
    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath);
    
    url <- paste(baseUrl, "_webdav", folderPath, fileSet, "/", remoteFilePath, sep="");
    
    options <- labkey.getRequestOptions(method="PUT")
    
    pbody <- upload_file(localFile)
    
    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE) {
      print(paste0("URL: ", url))
      response <- PUT(url=url, config=options, body=pbody, verbose(data_in=TRUE, info=TRUE, ssl=TRUE))
    } else {
      response <- PUT(url=url, config=options, body=pbody)
    }

    processResponse(response)
}

labkey.webdav.list <- function(baseUrl=NULL, file, folderPath, fileSet="@files")
{
  #TODO
}

labkey.webdav.downloadFolder <- function(baseUrl=NULL, file, folderPath, fileSet="@files", overwrite = TRUE)
{
  #TODO
}