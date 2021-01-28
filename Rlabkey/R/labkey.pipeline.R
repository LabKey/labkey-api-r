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
    if (missing(taskId) || is.null(taskId)) stop (paste("A value must be specified for taskId."))
    if (missing(protocolName) || is.null(protocolName)) stop (paste("A value must be specified for protocolName."))
    if (missing(path) || is.null(path)) stop (paste("A value must be specified for path."))
    if (missing(files) || is.null(files)) stop (paste("A value must be specified for files."))

    ## check parameter types
    if (!is.list(files)) stop (paste("The files parameter must be a list of strings."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    params <- list(taskId = taskId, protocolName = protocolName, path = path, file = files)

    url <- paste(baseUrl, "pipeline-analysis", folderPath, "getFileStatus.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response, simplifyVector=FALSE, simplifyDataFrame=FALSE))
}

labkey.pipeline.startAnalysis <- function(baseUrl=NULL, folderPath, taskId, protocolName, path, files,
    fileIds = list(), pipelineDescription = NULL, protocolDescription = NULL,
    jsonParameters = NULL, xmlParameters = NULL, allowNonExistentFiles = FALSE, saveProtocol = TRUE)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl)) stop (paste("A value must be specified for baseUrl."))
    if (missing(folderPath)) stop (paste("A value must be specified for folderPath."))
    if (missing(taskId) || is.null(taskId)) stop (paste("A value must be specified for taskId."))
    if (missing(protocolName) || is.null(protocolName)) stop (paste("A value must be specified for protocolName."))
    if (missing(path) || is.null(path)) stop (paste("A value must be specified for path."))
    if (missing(files) || is.null(files)) stop (paste("A value must be specified for files."))

    ## check parameter types
    if (!is.list(files)) stop (paste("The files parameter must be a list of strings."))
    if (!is.list(fileIds)) stop (paste("The fileIds parameter must be a list of data ids."))
    if (!is.null(xmlParameters) && !is.character(xmlParameters))
        stop (paste("The xml configuration is deprecated, please use the jsonParameters option to specify your protocol description."))
    if (!is.null(jsonParameters) && !(is.list(jsonParameters) || is.character(jsonParameters)))
        stop (paste("The jsonParameters parameter must be a list of key / value pairs or a string representation of that list created using toJSON."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    params <- list(taskId = taskId, protocolName = protocolName, path = path, file = files, fileIds = fileIds,
        allowNonExistentFiles = allowNonExistentFiles, saveProtocol = saveProtocol)

    ## only add the optional params to the URL if they are not NULL
    if (!is.null(pipelineDescription))
        params$pipelineDescription = pipelineDescription
    if (!is.null(protocolDescription))
        params$protocolDescription = protocolDescription
    if (!is.null(xmlParameters))
        params$configureXml = xmlParameters
    if (!is.null(jsonParameters))
    {
        params$configureJson = jsonParameters
        if (is.list(jsonParameters))
            params$configureJson = toJSON(jsonParameters, auto_unbox=TRUE)
    }

    url <- paste(baseUrl, "pipeline-analysis", folderPath, "startAnalysis.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE), haltOnError = FALSE)

    ## a successful response from this API call will contain a "status" property, so key off of that
    parsedResponse = fromJSON(response, simplifyVector=FALSE, simplifyDataFrame=FALSE)
    if (!is.null(parsedResponse$status))
        return (parsedResponse)

    message = parsedResponse$message
    if (!is.null(parsedResponse$exception))
        message <- parsedResponse$exception
    stop (paste("HTTP request was unsuccessful. Error message = ", message, sep=""))
}