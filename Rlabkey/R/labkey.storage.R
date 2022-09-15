##
#  Copyright (c) 2020 LabKey Corporation
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

labkey.storage.create <- function(baseUrl=NULL, folderPath, type, props)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(type) || missing(props))
        stop (paste("A value must be specified for each of baseUrl, folderPath, type, and props."))

    if (!is.list(props))
        stop (paste("Storage API props must be a list data structure."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    params <- list(type = type, props = props)
    url <- paste(baseUrl, "storage", folderPath, "create.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}

labkey.storage.update <- function(baseUrl=NULL, folderPath, type, props)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(type) || missing(props))
        stop (paste("A value must be specified for each of baseUrl, folderPath, type, and props."))

    if (!is.list(props))
        stop (paste("Storage API props must be a list data structure."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    params <- list(type = type, props = props)
    url <- paste(baseUrl, "storage", folderPath, "update.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}

labkey.storage.delete <- function(baseUrl=NULL, folderPath, type, rowId)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(type) || missing(rowId))
        stop (paste("A value must be specified for each of baseUrl, folderPath, type, and rowId."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    params <- list(type = type, props = list(rowId = rowId))
    url <- paste(baseUrl, "storage", folderPath, "delete.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}