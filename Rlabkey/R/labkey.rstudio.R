##
#  Copyright (c) 2010-2018 LabKey Corporation
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

## initialize a RStudio session for LabKey R report source editing
##
labkey.rstudio.initReport <- function(apiKey="", baseUrl="", folderPath, reportEntityId)
{
    labkey.setDefaults(apiKey, baseUrl);

    ## check required parameters
    if(missing(folderPath) || missing(reportEntityId))
        stop (paste("A value must be specified for each of folderPath and reportEntityId."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    url <- paste(baseUrl, "rstudio", folderPath, "getRReportContent.api", sep="")

    params <- list(entityId=reportEntityId)
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    result <- (fromJSON(response))

    if (result$success == TRUE)
    {
        ## reset working directory to home directory
        setwd('~/')

        ## create dir for report
        dir.create(file.path("LabKeyReports"), showWarnings = FALSE)
        dir.create(file.path("LabKeyReports", reportEntityId), showWarnings = FALSE)

        ## change working directory to report directory
        setwd(paste("LabKeyReports", reportEntityId, sep="/"))

        ## create props.JSON for folderPath
        labkey.rstudio.updateProp(reportEntityId, "folderPath", folderPath)
        labkey.rstudio.updateProp(reportEntityId, "lastModified", result$lastModified)

        ## create prolog script and update its content
        prologFileConn <- file("prolog.R", open="w")
        writeLines(result$prolog, prologFileConn)
        close(prologFileConn)

        ## create report file and update its content
        fileConn <- file(result$filename, open="w")
        writeLines(result$reportSource, fileConn)
        close(fileConn)

        ## open report for editing
        file.edit(result$filename)
    }
    else
    {
        stop(result$errorMsg)
    }
}

## Update RStudio report source back to LabKey
##
labkey.rstudio.saveReport <- function(folderPath, reportEntityId, lkLastModified)
{
    ## check required parameters
    if(missing(reportEntityId))
        stop (paste("A value must be specified for reportEntityId."))

    ## check working directory
    if (!grepl(reportEntityId, getwd()))
        stop (paste("Working directory is currently not set to report's directory. Skip saving source to LabKey."))

    if(missing(folderPath))
    {
        folderPath <- labkey.rstudio.getSavedProp(reportEntityId, "folderPath")
        if (missing(folderPath))
            stop (paste("Unable to determine folderPath"))
    }

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    if (!missing(lkLastModified))
    {
        localLastModified <- labkey.rstudio.getSavedProp(reportEntityId, "lastModified")
        if (localLastModified == lkLastModified)
        {
            skipSaving <- .rs.api.showQuestion("Save to LabKey?", "Do you want to update report content to LabKey server?")
        }
        else
        {
            skipSaving <- .rs.api.showQuestion("Save to LabKey? (Potential conflicting edit)", "The report source was modified in LabKey and the content might have diverged from local copy. Do you want to save local changes to LabKey server?")
        }
        if (!skipSaving)
        {
            stop (paste("Skipped saving updated source to LabKey server"))
            return;
        }
    }

    url <- paste(baseUrl, "rstudio", folderPath, "SaveRReportContent.api", sep="")

    params <- list(entityId=reportEntityId)
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    result <- (fromJSON(response))

    if (result$success != TRUE)
        stop(result$errorMsg)

}

## Read property value form prop.txt
##
labkey.rstudio.getSavedProp <- function(reportEntityId, propName)
{
    propsFilepath <- "props.JSON"
    if (!file.exists(propsFilepath))
        return (NULL)
    props <- fromJSON(txt = propsFilepath)
    return (props[propName])
}

## Update property value to prop.txt
##
labkey.rstudio.updateProp <- function(reportEntityId, propName, propValue)
{
    propsFilepath <- "props.JSON"
    if (!file.exists(propsFilepath))
        props <- list()
    else
        props <- fromJSON(txt = propsFilepath)
    props[propName] = propValue
    write(toJSON(props), file=propsFilepath)
}