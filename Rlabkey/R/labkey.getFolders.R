##
# Copyright (c) 2010-2018 LabKey Corporation
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
##

labkey.getFolders <- function(baseUrl=NULL, folderPath, includeEffectivePermissions=TRUE, includeSubfolders=FALSE, depth=50,
    includeChildWorkbooks=TRUE, includeStandardProperties=TRUE)
{
	baseUrl=labkey.getBaseUrl(baseUrl)

    ## Validate required parameters
    if (missing(folderPath)) stop (paste("A value must be specified for folderPath."))

	## normalize the folder path
	folderPath <- encodeFolderPath(folderPath)

	## Formatting
	if(includeSubfolders) {inclsf <- paste("1&depth=", depth, sep="")} else {inclsf <- "0"}
	if(includeEffectivePermissions) {inclep <- "1"} else {inclep <- "0"}
	if(includeChildWorkbooks) {inclcw <- "1"} else {inclcw <- "0"}

	inclsp <- "0"
	resultCols = c("name", "path", "id", "effectivePermissions")
	if(includeStandardProperties) {
	    inclsp <- "1"
	    resultCols = c("name", "path", "id", "title", "type", "folderType", "effectivePermissions")
	}

	## Construct url
	myurl <- paste(baseUrl,"project",folderPath,"getContainers.view?","includeSubfolders=",inclsf,
	    "&includeEffectivePermissions=",inclep,"&includeChildWorkbooks=",inclcw,"&includeStandardProperties=",inclsp,
	    sep="")

	## Execute via our standard GET function
	mydata <- labkey.get(myurl);

	decode <- fromJSON(mydata, simplifyVector=FALSE, simplifyDataFrame=FALSE)

	curfld <- decode
	curfld$effectivePermissions = paste(curfld$effectivePermissions, collapse=",")

    # Issue 44619: account for missing properties if the parent is root
    for (col in resultCols) {
        if (is.null(curfld[[col]])) {
            curfld[[col]] = "";
        }
    }

	allpaths <- matrix(data=unlist(curfld[resultCols]), nrow=1, ncol=length(resultCols), byrow=TRUE)
	childflds <- curfld$children[]
	while (length(childflds)>0)
	{
		curfld<-childflds[1][[1]]
		curfld$effectivePermissions = paste(curfld$effectivePermissions, collapse=",")
		allpaths <- rbind(allpaths, unlist(curfld[resultCols]))
		childflds <- c(childflds, curfld$children[])
		childflds <- childflds[-1]
	}

	allpathsDF <- data.frame(allpaths, stringsAsFactors=FALSE)
	if(includeStandardProperties) {
	    colnames(allpathsDF) <- c("name", "folderPath", "id", "title", "type", "folderType", "effectivePermissions")
    } else {
        colnames(allpathsDF) <- c("name", "folderPath", "id", "effectivePermissions")
    }

	return(allpathsDF)
}