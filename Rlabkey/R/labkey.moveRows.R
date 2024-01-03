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

labkey.moveRows <- function(baseUrl=NULL, folderPath, targetFolderPath, schemaName, queryName, toMove, options = NULL)
{  
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## Validate required parameters
    if (missing(folderPath)) stop (paste("A value must be specified for folderPath."))
    if (missing(targetFolderPath)) stop (paste("A value must be specified for targetFolderPath."))
    if (missing(schemaName)) stop (paste("A value must be specified for schemaName."))
    if (missing(queryName)) stop (paste("A value must be specified for queryName."))
    if (missing(toMove)) stop (paste("A value must be specified for toMove."))
    if (!missing(options) & !is.list(options))
        stop (paste("The options parameter must be a list data structure."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    ## URL encode folder path, JSON encode post body (if not already encoded)
    toMove <- convertFactorsToStrings(toMove);

    params <- list(targetContainerPath=targetFolderPath, schemaName=schemaName, queryName=queryName, apiVersion=8.3)
    if (!missing(options))
        params <- c(params, options)

    pbody <- jsonEncodeRowsAndParams(toMove, params, NULL)

    myurl <- paste(baseUrl, "query", folderPath, "moveRows.api", sep="")

    ## Execute via our standard POST function
    mydata <- labkey.post(myurl, pbody)
    newdata <- fromJSON(mydata, simplifyVector=FALSE, simplifyDataFrame=FALSE)

    return(newdata)
}
                                                              
