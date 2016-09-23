##
#  Copyright (c) 2008-2015 Fred Hutchinson Cancer Research Center
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

labkey.executeSql <- function(baseUrl=NULL, folderPath, schemaName, sql, maxRows=NULL,
        rowOffset=NULL, colSort=NULL, showHidden=FALSE, colNameOpt='caption',
        containerFilter=NULL, parameters=NULL)
{
baseUrl=labkey.getBaseUrl(baseUrl)

## Error if any of baseUrl, folderPath, schemaName or sql are missing
if(exists("baseUrl")==FALSE || is.null(baseUrl) || exists("folderPath")==FALSE || exists("schemaName")==FALSE || exists("sql")==FALSE)
    stop (paste("A value must be specified for each of baseUrl, folderPath, schemaName and sql."))

## URL encoding of folder path (if not already encoded)
if(folderPath==URLdecode(folderPath)) {folderPath <- URLencode(folderPath)}

## Formatting
baseUrl <- gsub("[\\]", "/", baseUrl)
folderPath <- gsub("[\\]", "/", folderPath)
if(substr(baseUrl, nchar(baseUrl), nchar(baseUrl))!="/"){baseUrl <- paste(baseUrl,"/",sep="")}
if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/"){folderPath <- paste(folderPath,"/",sep="")}
if(substr(folderPath, 1, 1)!="/"){folderPath <- paste("/",folderPath,sep="")}

## Construct url
myurl <- paste(baseUrl, "query", folderPath, "executeSql.api", sep="")

## Construct parameters
params <- list(schemaName=schemaName, apiVersion=8.3, sql=sql)
if(is.null(maxRows)==FALSE) {params <- c(params, list(maxRows=maxRows))}
if(is.null(maxRows)==TRUE) {params <- c(params, list(showRows="all"))}
if(is.null(rowOffset)==FALSE) {params <- c(params, list(offset=rowOffset))}
if(is.null(colSort)==FALSE) {params <- c(params, list(query.sort=colSort))}
if(is.null(parameters)==FALSE) {for(k in 1:length(parameters)) params <- c(params, list("query.param."=parameters[k]))}
if(is.null(containerFilter)==FALSE) {params <- paste(params, list("containerFilter"=containerFilter))}

pbody <- toJSON(params)

## Execute via our standard POST function
mydata <- labkey.post(myurl, pbody)

newdata <- makeDF(rawdata=mydata, showHidden=showHidden, colNameOpt=colNameOpt)

return(newdata)
}

