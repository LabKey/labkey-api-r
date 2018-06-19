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

## Returns the domain design (as a dataframe) for the specified domain
##
labkey.domain.get <- function(baseUrl=NULL, folderPath, schemaName, queryName)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if(missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(schemaName) || missing(queryName))
        stop (paste("A value must be specified for each of baseUrl, folderPath, schemaName and queryName."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    url <- paste(baseUrl, "property", folderPath, "getDomain.api", sep="")

    params <- list(schemaName=schemaName, queryName=queryName)
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}

## Update an existing domain
##
labkey.domain.save <- function(baseUrl=NULL, folderPath, schemaName, queryName, domainDesign)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(schemaName) || missing(queryName) || missing(domainDesign))
        stop (paste("A value must be specified for each of baseUrl, folderPath, schemaName, queryName and domainDesign."))

    if (!is.list(domainDesign))
        stop (paste("domainDesign must be a list data structure."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)
    params <- list(schemaName = schemaName, queryName = queryName, domainDesign = domainDesign)

    url <- paste(baseUrl, "property", folderPath, "saveDomain.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}

## Helper function to create the domain design list
##
labkey.domain.createDesign <- function(name, description, fields)
{
    ## check required parameters
    if (missing(name) || missing(fields))
        stop (paste("A value must be specified for each of name and fields."))

    if (!is.list(fields))
        stop (paste("fields must be a list of field definitions."))

    dd <- list(name = name, fields = fields$fields)
    if (!missing(description))
        dd$description = description

    return (dd)
}

labkey.domain.create <- function(baseUrl=NULL, folderPath, domainKind=NULL, domainDesign=NULL, options=NULL,
        module=NULL, domainGroup=NULL, domainTemplate=NULL, createDomain=TRUE, importData=TRUE)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath))
        stop (paste("A value must be specified for each of baseUrl and folderPath."))

    if (is.null(domainKind) && is.null(domainTemplate))
        stop (paste("Domain creation must use either a domain kind or a domain template."))

    if (!is.null(domainKind))
    {
        if (is.null(domainDesign))
            stop (paste("If domainKind is specified, then domainDesign must also be included."))

        if (!is.list(domainDesign))
            stop (paste("domainDesign must be a list data structure."))

        params <- list(kind = domainKind, domainDesign = domainDesign)
        if (!missing(options))
        {
            if (!is.list(options))
                stop (paste("options must be a list data structure."))
            params$options = options
        }
    }

    if (!is.null(domainTemplate))
    {
        if (is.null(module) || is.null(domainGroup))
            stop (paste("If domainTemplate is specified, module and domainGroup are required."))

        params <- list(domainTemplate = domainTemplate, module = module, domainGroup = domainGroup,
                createDomain = createDomain, importData = importData)
    }

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    url <- paste(baseUrl, "property", folderPath, "createDomain.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}

labkey.domain.drop <- function(baseUrl=NULL, folderPath, schemaName, queryName)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(schemaName) || missing(queryName))
        stop (paste("A value must be specified for each of baseUrl, folderPath, schemaName and queryName."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    url <- paste(baseUrl, "property", folderPath, "deleteDomain.api", sep="")

    params <- list(schemaName=schemaName, queryName=queryName)
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}

labkey.domain.inferFields <- function(baseUrl=NULL, folderPath, df)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(df))
        stop (paste("A value must be specified for each of baseUrl, folderPath and df."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    ## write the dataframe to a tempfile to post to the server
    tf <- tempfile(fileext=".tsv")
    write.table(df, file=tf, sep="\t", quote=FALSE, row.names=FALSE)

    ## Execute via our standard POST function
    url <- paste(baseUrl, "property", folderPath, "inferDomain.api", sep="")

    rawdata <- labkey.post(url, list(file=upload_file(tf)), encoding="multipart")
    ## delete the temp file
    file.remove(tf)
    response <- fromJSON(rawdata)

    return (response)
}

labkey.domain.createAndLoad <- function(baseUrl=NULL, folderPath, name, description="", df, domainKind, options=NULL, schemaName=NULL)
{
    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(name) || missing(df) || missing(domainKind))
        stop (paste("A value must be specified for each of baseUrl, folderPath, name, df or domainKind."))

    if (is.null(options))
        options <- list(strictFieldValidation = FALSE)
    else
        options <- c(options, list(strictFieldValidation = FALSE))

    if (is.null(schemaName))
    {
        if (domainKind == "StudyDatasetVisit" || domainKind == "StudyDatatsetDate")
            schemaName <- "study"
        else if (domainKind == "IntList" || domainKind == "VarList")
            schemaName <- "lists"
        else if (domainKind == "IssueDefinition")
            schemaName <- "issues"
        else if (domainKind == "SampleSet")
            schemaName <- "samples"
        else if (domainKind == "DataClass")
            schemaName <- "exp.data"
    }

    if (is.null(schemaName))
        stop (paste("A value must be specified for schemaName."))

    fields = labkey.domain.inferFields(baseUrl = baseUrl, folderPath = folderPath, df = df[,colnames(df)])

    design = labkey.domain.createDesign( fields = fields, name = name, description = description)
    labkey.domain.create(baseUrl = baseUrl, folderPath = folderPath, domainKind = domainKind,
        domainDesign = design, options = options)

    labkey.insertRows(baseUrl = baseUrl, folderPath = folderPath, schemaName = schemaName, queryName= name, df)
}
