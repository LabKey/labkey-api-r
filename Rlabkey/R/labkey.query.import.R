##
#  Copyright (c) 2013-2018 LabKey Corporation
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

labkey.query.import <- function(baseUrl=NULL, folderPath, schemaName, queryName, toImport, options = NULL)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(schemaName) || missing(queryName) || missing(toImport))
        stop (paste("A value must be specified for each of baseUrl, folderPath, schemaName, queryName, and toImport."))
    if (!missing(options) & !is.list(options))
        stop (paste("The options parameter must be a list data structure."))

    ## write the dataframe to a tempfile to post to the server
    tf <- tempfile(fileext=".tsv")
    write.table(toImport, file=tf, sep="\t", quote=FALSE, row.names=FALSE)

    ## build the options param list
    if (is.null(options))
        options <- list()
    options <- c(options, list(schemaName=schemaName, queryName=queryName, file=upload_file(tf)))

    ## Execute via our standard POST function
    url <- labkey.buildURL(baseUrl, "query", "import.api", folderPath)
    rawdata <- labkey.post(url, options, encoding="multipart")
    response <- fromJSON(rawdata, simplifyVector=FALSE, simplifyDataFrame=FALSE)

    ## delete the temp file
    file.remove(tf)

    return(response)
}

