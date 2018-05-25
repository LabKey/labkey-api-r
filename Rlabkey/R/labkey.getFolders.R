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

labkey.getFolders <- function(baseUrl=NULL, folderPath, includeEffectivePermissions=TRUE, includeSubfolders=FALSE, depth=50)
{
	baseUrl=labkey.getBaseUrl(baseUrl)

	## Empty string/NULL checking
	if(missing("baseUrl") || is.null(baseUrl) || missing("folderPath"))
		stop (paste("A value must be specified for both baseUrl and folderPath"))

	## normalize the folder path
	folderPath <- encodeFolderPath(folderPath)

	## Formatting
	if(includeSubfolders) {inclsf <- paste("1&depth=", depth, sep="")} else {inclsf <- "0"}
	if(includeEffectivePermissions) {inclep <- "1"} else {inclep <- "0"}

	## Construct url
	myurl <- paste(baseUrl,"project",folderPath,"getContainers.view?","includeSubfolders=",inclsf,"&includeEffectivePermissions=",inclep, sep="")

	## Execute via our standard GET function
	mydata <- labkey.get(myurl);

	decode <- fromJSON(mydata, simplifyVector=FALSE, simplifyDataFrame=FALSE)
	curfld <- decode
	allpaths <- matrix(data=c(curfld$name, curfld$path, paste(curfld$effectivePermissions, collapse=",")), nrow=1, ncol=3, byrow=TRUE)
	todo <- curfld$children[]
	while (length(todo)>0)
	{
		curfld<-todo[1][[1]]
		allpaths <- rbind(allpaths, c(curfld$name, curfld$path, paste(curfld$effectivePermissions, collapse=",")))
		todo<- c(todo, curfld$children[])
		todo<-todo[-1]
	}

	allpathsDF <- data.frame(allpaths, stringsAsFactors=FALSE)
	colnames(allpathsDF) <- c("name", "folderPath", "effectivePermissions")

	return(allpathsDF)
}