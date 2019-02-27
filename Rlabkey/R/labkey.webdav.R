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

labkey.webdav.get <- function(baseUrl=NULL, folderPath, fileSet="@files", filePath, overwrite=TRUE)
{
    # dont bother querying if this file already exists, since we wont overwrite it
    if (!overwrite & file.exists(filePath)) {
      return()
    }

    baseUrl=labkey.getBaseUrl(baseUrl);

    ## check required parameters
    if(missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(fileSet) || missing(filePath))
        stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, and filePath."));

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath);

    url <- paste(baseUrl, "_webdav", folderPath, fileSet, "/", filePath, sep="");

    options <- labkey.getRequestOptions(method="GET")
    
    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE)
      response <- GET(url=myurl, write_disk(filePath, overwrite=overwrite), config=options, verbose(data_in=TRUE, info=TRUE, ssl=TRUE))
    else
      response <- GET(url=myurl, write_disk(filePath, overwrite=overwrite), config=options)
    processResponse(response)
}


labkey.webdav.put <- function(baseUrl=NULL, file, folderPath, fileSet="@files", filePath, overwriteRemote=TRUE)
{
    baseUrl=labkey.getBaseUrl(baseUrl);
    
    ## check required parameters
    if(missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(fileSet) || missing(filePath))
      stop (paste("A value must be specified for each of baseUrl, folderPath, fileSet, and filePath."));
    
    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath);
    
    url <- paste(baseUrl, "_webdav", folderPath, fileSet, "/", filePath, sep="");
    
    options <- labkey.getRequestOptions(method="PUT")
    
    pbody <- upload_file(system.file(filePath))
    
    if (!is.null(.lkdefaults[["debug"]]) && .lkdefaults[["debug"]] == TRUE)
      response <- POST(url=myurl, config=options, body=pbody, verbose(data_in=TRUE, info=TRUE, ssl=TRUE))
    else
      response <- POST(url=myurl, config=options, body=pbody)

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