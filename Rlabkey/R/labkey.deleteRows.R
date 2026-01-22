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

labkey.deleteRows <- function(baseUrl=NULL, folderPath, schemaName, queryName, toDelete, provenanceParams=NULL, options = NULL)
{  
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## Default showAllRows=TRUE
    showAllRows=TRUE

    ## Validate required parameters
    if (missing(folderPath)) stop (paste("A value must be specified for folderPath."))
    if (missing(schemaName)) stop (paste("A value must be specified for schemaName."))
    if (missing(queryName)) stop (paste("A value must be specified for queryName."))
    if (missing(toDelete)) stop (paste("A value must be specified for toDelete."))
    if (nrow(toDelete) == 0) stop (paste("toDelete must contain at least one row."))
    if (!missing(options) & !is.list(options))
        stop (paste("The options parameter must be a list data structure."))

    ## URL encode folder path, JSON encode post body (if not already encoded)
    toDelete <- convertFactorsToStrings(toDelete);

    params <- list(schemaName=schemaName, queryName=queryName, apiVersion=8.3)
    if (!missing(provenanceParams))
        params$provenance = provenanceParams
    if (!missing(options))
        params <- c(params, options)

    pbody <- jsonEncodeRowsAndParams(toDelete, params, NULL)
    myurl <- labkey.buildURL(baseUrl, "query", "deleteRows.api", folderPath)

    ## Execute via our standard POST function
    mydata <- labkey.post(myurl, pbody)
    newdata <- fromJSON(mydata, simplifyVector=FALSE, simplifyDataFrame=FALSE)

    return(newdata)
}
                                                              
labkey.truncateTable <- function(baseUrl=NULL, folderPath, schemaName, queryName)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath))
        stop (paste("A value must be specified for each of baseUrl and folderPath."))

    ## Validate required parameters
    if (missing(schemaName)) stop (paste("A value must be specified for schemaName."))
    if (missing(queryName)) stop (paste("A value must be specified for queryName."))

    url <- labkey.buildURL(baseUrl, "query", "truncateTable.api", folderPath)

    params <- list(schemaName=schemaName, queryName=queryName)
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}
