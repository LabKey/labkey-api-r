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

labkey.selectRows <- function(baseUrl=NULL, folderPath, schemaName, queryName, viewName=NULL, colSelect=NULL,
        maxRows=NULL, rowOffset=NULL, colSort=NULL, colFilter=NULL, showHidden=FALSE, colNameOpt='caption',
        containerFilter=NULL, parameters=NULL, includeDisplayValues=FALSE, method='POST')
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    # Empty string/NULL checking
    if (!is.null(viewName)) {char <- nchar(viewName); if(char<1){viewName<-NULL}}
    if (!is.null(colSelect)) {char <- nchar(colSelect[1]); if(char<1){colSelect<-NULL}}
    if (!is.null(maxRows)) {char <- nchar(maxRows); if(char<1){maxRows<-NULL}}
    if (!is.null(rowOffset)) {char <- nchar(rowOffset); if(char<1){rowOffset<-NULL}}
    if (!is.null(colSort)) {char <- nchar(colSort); if(char<1){colSort<-NULL}}
    if (!is.null(colFilter)) {char <- nchar(colFilter[1]); if(char<1){colFilter<-NULL}}
    if (!is.null(showHidden)) {char <- nchar(showHidden); if(char<1){showHidden<-FALSE}}
    if (!is.null(containerFilter)) {char <- nchar(containerFilter[1]); if(char<1){containerFilter<-NULL}}
    if (!is.null(parameters)) {char <- nchar(parameters[1]); if(char<1){parameters<-NULL}}
    if (!is.null(includeDisplayValues)) {char <- nchar(includeDisplayValues); if(char<1){includeDisplayValues<-FALSE}}

    # Validate required parameters
    if (missing(folderPath)) stop (paste("A value must be specified for folderPath."))
    if (missing(schemaName)) stop (paste("A value must be specified for schemaName."))
    if (missing(queryName)) stop (paste("A value must be specified for queryName."))

    # Format colSelect
    colSelect2=NULL
    if (!is.null(colSelect))
    {
        lencolSel <- length(colSelect)
        holder <- NULL
        for (i in 1:length(colSelect))
            holder <-paste(holder, colSelect[i],",",sep="")
        colSelect2 <- substr(holder, 1, nchar(holder)-1)
        colSelect <- paste(colSelect, collapse=",")

        # when using colSelect, always set showHidden to TRUE
        showHidden = TRUE
    }

    # Construct the query parameter list of named elements (key / value pairs)
    params <- list("schemaName"=schemaName, "query.queryName"=queryName, "apiVersion"="8.3")
    if (!is.null(includeDisplayValues) && includeDisplayValues == TRUE)
        params <- c(params, list("includeDisplayValues"=includeDisplayValues))
    if (!is.null(viewName))
        params <- c(params, list("query.viewName"=viewName))
    if (!is.null(colSelect2))
        params <- c(params, list("query.columns"=colSelect2))
    if (!is.null(maxRows))
        params <- c(params, list("query.maxRows"=maxRows))
    if (is.null(maxRows))
        params <- c(params, list("query.maxRows"=-1))
    if (!is.null(rowOffset))
        params <- c(params, list("query.offset"=rowOffset))
    if (!is.null(colSort))
        params <- c(params, list("query.sort"=colSort))
    if (!is.null(colFilter))
    {
        if (is.list(colFilter) && !is.null(names(colFilter)))
        {
            # preferred list with named elements format
            params <- c(params, colFilter)
        }
        else if (length(colFilter) > 0)
        {
            # Legacy format of URL encoded key / value pairs, convert to a list of named elements
            # which can be processed by buildURL
            params <- c(params, parseToList(colFilter, dataRegionName="", urlDecode=TRUE))
        }
        else
            stop (paste("Argument colFilter must be a list or vector generated from makeFilter"))
    }
    if (!is.null(parameters))
    {
        # Support the legacy format. TODO: require a list with named elements that can
        # be passed directly to the larger param list without needing to parse
        params <- c(params, parseToList(parameters, dataRegionName="query.param."))
    }
    if (!is.null(containerFilter))
        params <- c(params, list("containerFilter"=containerFilter))

    if (!is.null(method) && method == "GET")
    {
        # Execute via our standard GET function
        myurl <- labkey.buildURL(baseUrl, "query", "selectRows.api", folderPath, params)
        mydata <- labkey.get(myurl);
    }
    else
    {
        # Execute via our standard POST function
        myurl <- labkey.buildURL(baseUrl, "query", "selectRows.api", folderPath)
        mydata <- labkey.post(myurl, toJSON(params, auto_unbox=TRUE))
    }

    newdata <- makeDF(mydata, colSelect, showHidden, colNameOpt)

    # Check for less columns returned than requested
    if(is.null(colSelect)==FALSE){if(ncol(newdata)<lencolSel)warning("Fewer columns are returned than were requested in the colSelect variable. The column names may be invalid. Be sure to use the column name and not the column caption. See the documentation for further explaination.")}

    return(newdata)
}

# Utility to convert a vector of parameter values of the form : "foo=bar" into a list with
# named elements which is the format that buildURL requires for parameters.
#
parseToList <- function(parameters, dataRegionName="query.", urlDecode=FALSE)
{
    params <- list()
    for (i in 1:length(parameters))
    {
        # find the last occurrence of "=" to split on
        parts <- gregexpr("=", parameters[i], fixed = TRUE)[[1]]
        idx <- parts[length(parts)]

        if (idx != -1)
        {
            key <- substr(parameters[i], 1, idx - 1)
            value <- substr(parameters[i], idx + 1, nchar(parameters[i]))

            if (urlDecode)
            {
                key <- URLdecode(key)
                value <- URLdecode(value)
            }
            paramList <- list(value)
            names(paramList) <- paste(dataRegionName, key, sep="")
            params <- c(params, paramList)
        }
        else
            stop (paste("Argument parameters is incorrectly formatted, it needs to be a list of string value pairs delimited by '='"))
    }
    return (params)
}


