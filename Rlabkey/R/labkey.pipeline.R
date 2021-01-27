##
#  Copyright (c) 2018 LabKey Corporation
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

labkey.pipeline.getPipelineContainer <- function(baseUrl=NULL, folderPath)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl)) stop (paste("A value must be specified for baseUrl."))
    if (missing(folderPath)) stop (paste("A value must be specified for folderPath."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    url <- paste(baseUrl, "pipeline", folderPath, "getPipelineContainer.api", sep="")
    response <- labkey.get(url)

    return (fromJSON(response, simplifyVector=FALSE, simplifyDataFrame=FALSE))
}

labkey.pipeline.getProtocols <- function(baseUrl=NULL, folderPath, taskId, path, includeWorkbooks = FALSE)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl)) stop (paste("A value must be specified for baseUrl."))
    if (missing(folderPath)) stop (paste("A value must be specified for folderPath."))
    if (missing(taskId) || is.null(taskId)) stop (paste("A value must be specified for taskId."))
    if (missing(path) || is.null(path)) stop (paste("A value must be specified for path."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    params <- list(taskId = taskId, path = path, includeWorkbooks = includeWorkbooks)
    url <- paste(baseUrl, "pipeline-analysis", folderPath, "getSavedProtocols.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response, simplifyVector=FALSE, simplifyDataFrame=FALSE))
}

labkey.pipeline.getFileStatus <- function(baseUrl=NULL, folderPath, taskId, protocolName, path, files)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl)) stop (paste("A value must be specified for baseUrl."))
    if (missing(folderPath)) stop (paste("A value must be specified for folderPath."))
    if (missing(protocolName) || is.null(protocolName)) stop (paste("A value must be specified for protocolName."))
    if (missing(taskId) || is.null(taskId)) stop (paste("A value must be specified for taskId."))
    if (missing(path) || is.null(path)) stop (paste("A value must be specified for path."))
    if (missing(files) || is.null(files)) stop (paste("A value must be specified for files."))
    if (!is.list(files)) stop (paste("The files parameter must be a list of strings."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    params <- list(taskId = taskId, protocolName = protocolName, path = path, file = files)
    url <- paste(baseUrl, "pipeline-analysis", folderPath, "getFileStatus.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response, simplifyVector=FALSE, simplifyDataFrame=FALSE))
}

labkey.pipeline.startAnalysis <- function(baseUrl=NULL, folderPath)
{

}