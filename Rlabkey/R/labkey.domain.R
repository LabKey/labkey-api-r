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

# temporary
testInferFields <- function(baseUrl, folderPath)
{
    df <- data.frame(ptid=c(1:3), age = c(10,20,30), sex = c("f", "m", "f"))
    labkey.inferFields(baseUrl, folderPath, df)
}

labkey.inferFields <- function(baseUrl=NULL, folderPath, df)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if(exists("baseUrl")==FALSE || is.null(baseUrl) || exists("folderPath")==FALSE)
        stop (paste("A value must be specified for each of baseUrl, folderPath."))

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

    decode <- fromJSON(rawdata)

    ## TODO, need to deserialize from json to the appropriate data structure.
}
