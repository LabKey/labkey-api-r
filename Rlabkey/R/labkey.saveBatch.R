##
#  Copyright (c) 2010-2017 LabKey Corporation
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

labkey.saveBatch <- function(baseUrl=NULL, folderPath, assayName, resultDataFrame, batchPropertyList=NULL, runPropertyList=NULL)
{
baseUrl = labkey.getBaseUrl(baseUrl)

## Error if any of baseUrl, folderPathare missing
if(exists("baseUrl")==FALSE || is.null(baseUrl) || exists("folderPath")==FALSE || exists("assayName")==FALSE  || exists("resultDataFrame")==FALSE)
stop (paste("A value must be specified for each of baseUrl, folderPath, assayName, and resultDataFrame"))
## TODO.check for at least one of the instert blocks is not null


## Formatting
baseUrl <- gsub("[\\]", "/", baseUrl)
folderPath <- gsub("[\\]", "/", folderPath)
if(substr(baseUrl, nchar(baseUrl), nchar(baseUrl))!="/"){baseUrl <- paste(baseUrl,"/",sep="")}
if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/"){folderPath <- paste(folderPath,"/",sep="")}
if(substr(folderPath, 1, 1)!="/"){folderPath <- paste("/",folderPath,sep="")}

## URL encode folder path and assay name (if not already encoded) 
if(folderPath!=URLencode(folderPath)) {folderPath <- URLencode(folderPath)}
if(assayName==URLdecode(assayName)) {assayNameParam <- URLencode(assayName)}
else {assayNameParam <- assayName}

## Translate assay name to an ID
myurl <- paste(baseUrl,"assay",folderPath,"assayList.view?name=", assayNameParam, sep="")

## Execute via our standard GET function
assayInfoJSON <- labkey.get(myurl)

assayDef <- NULL
assayInfo<- fromJSON(assayInfoJSON)
if (length(assayInfo) == 1 && length(assayInfo[[1]]) == 1)
{
	assayDef <- assayInfo[[1]][[1]]
	if (assayDef$name != assayName)
		{assayDef <- NULL}
	## TODO:  check assay domain def against dataframe	
}
if (is.null(assayDef))
	{stop(paste("Could not find an assay matching that name." ,sep=""))}
	
# build Assay object tree based on R lists
nrows <- nrow(resultDataFrame)
ncols <- ncol(resultDataFrame)
cnames <- colnames(resultDataFrame)
rowsVector <- vector(mode="list", length=nrows)
for(j in 1:nrows) {
	cvalues <- as.list(resultDataFrame[j,])
	names(cvalues) <- cnames
	rowsVector[[j]] <- cvalues
}

dataInputsArray <- vector(mode="list", length=0)

runsArray <- vector(mode="list", length=1)
runPropertyList <- c(runPropertyList, list("dataInputs" = dataInputsArray))

runsArray[[1]] <- c(runPropertyList, list("dataRows" = rowsVector))

batchPropertyList <- c(batchPropertyList, list("runs" = runsArray))
	
baseAssayList <- list(assayId=assayDef$id)
baseAssayList <- c(baseAssayList, list(batch=batchPropertyList))

## Now post form with batch object filled out
myurl <- paste(baseUrl, "assay", folderPath, "saveAssayBatch.view", sep="")
pbody <- toJSON(baseAssayList)

## Execute via our standard POST function
mydata <- labkey.post(myurl, pbody)
newAssayInfo <- fromJSON(mydata)

return(newAssayInfo)
}
                                                              
