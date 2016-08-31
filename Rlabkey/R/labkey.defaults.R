##
#  Copyright (c) 2016 LabKey Corporation
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

.lkdefaults <- new.env(parent=emptyenv());

# Set the credentials used for all http or https requests
# For now, just apiKey. TODO: Add email, password, folderPath, and maybe curlOptions & lkOptions
labkey.setDefaults <- function(apiKey="",baseUrl="")
{
    if (apiKey != "")
        .lkdefaults[["apiKey"]] = apiKey;
    if (baseUrl != "")
        .lkdefaults[["baseUrl"]] = baseUrl;
    # clear defaults if setDefaults() is called with NO arguments
    if (apiKey == "" && baseUrl=="")
      .lkdefaults <- new.env(parent=emptyenv());
}

ifApiKey <- function()
{
    if (exists("labkey.apiKey", envir = .GlobalEnv)) {
        get("labkey.apiKey", envir = .GlobalEnv)
    } else {
        .lkdefaults[["apiKey"]];
    }
}

labkey.getBaseUrl <- function(baseUrl=NULL)
{
    if (!is.null(baseUrl) && baseUrl != "") {
      return (baseUrl)
    } else {
        return (.lkdefaults[["baseUrl"]])
    }
}

## Executes an HTTP GET against the supplied URL, with standard handling for session, api key, status codes and error messages.
labkey.get <- function(myurl)
{
    ## Set options
    reader <- basicTextGatherer()
    header <- basicTextGatherer()
    myopts <- curlOptions(writefunction=reader$update, headerfunction=header$update, .opts=c(labkey.curlOptions()))

    ## Support user-settable options for debugging and setting proxies etc
    if(exists(".lksession"))
    {
        userOpt <- .lksession[["curlOptions"]]
        if (!is.null(userOpt))
            {myopts<- curlOptions(.opts=c(myopts, userOpt))}
    }

    ## HTTP GET
    handle <- getCurlHandle()
    clist <- ifcookie()
    if(clist$Cvalue==1)
    {
        mydata <- getURI(myurl, .opts=myopts, cookie=paste(clist$Cname, "=", clist$Ccont, sep=""))
    } else {
        myopts <- curlOptions(.opts=c(myopts, httpauth=1L))
        apikey <- ifApiKey();

        if (!is.null(apikey))
        {
            mydata <- getURI(myurl, .opts=myopts, curl=handle, httpheader = c("apikey"=apikey))
        } else {
            myopts <- curlOptions(.opts=c(myopts, netrc=1))
            mydata <- getURI(myurl, .opts=myopts, curl=handle)
        }
    }

    processResponse(mydata, header, handle)
}

## Executes an HTTP POST of pbody against the supplied URL, with standard handling for session, api key, status codes and error messages.
labkey.post <- function(myurl, pbody)
{
    ## Set options
    reader <- basicTextGatherer()
    header <- basicTextGatherer()
    handle <- getCurlHandle()
    headerFields <- c('Content-Type'="application/json;charset=utf-8")
    clist <- ifcookie()
    if(clist$Cvalue==1) {
        myopts <- curlOptions(cookie=paste(clist$Cname, "=", clist$Ccont, sep=""), writefunction=reader$update, headerfunction=header$update, .opts=c(labkey.curlOptions()))
    } else {
        apikey <- ifApiKey();

        if (!is.null(apikey))
        {
            myopts <- curlOptions(writefunction=reader$update, headerfunction=header$update, .opts=c(labkey.curlOptions()))
            headerFields <- c(headerFields, apikey=apikey)
        } else {
            myopts <- curlOptions(netrc=1, writefunction=reader$update, headerfunction=header$update, .opts=c(labkey.curlOptions()))
        }
    }

    ## Support user-settable options for debugging and setting proxies etc
    if(exists(".lksession"))
    {
        userOpt <- .lksession[["curlOptions"]]

        if (!is.null(userOpt))
        {
            myopts <- curlOptions(.opts=c(myopts, userOpt))
        }
    }

    ## HTTP POST form
    curlPerform(url=myurl, postFields=pbody, httpheader=headerFields, .opts=myopts, curl=handle)
    mydata <- reader$value();

    processResponse(mydata, header, handle)
}

processResponse <- function(mydata, header, handle)
{
    ## Error checking, decode data and return
    h <- parseHeader(header$value())
    message <- h$statusMessage
    status <- getCurlInfo(handle)$response.code
    if (status == 0) {
        status <- h$status
    }

    if(status==500)
    {
        decode <- fromJSON(mydata)
        message <- decode$exception
        stop(paste("HTTP request was unsuccessful. Status code = ", status, ", Error message = ", message, sep=""))
    }

    if(status>=400)
    {
        contTypes <- which(names(h)=='Content-Type')
        if(length(contTypes)>0 && (tolower(h[contTypes[1]])=="application/json;charset=utf-8" || tolower(h[contTypes[2]])=="application/json;charset=utf-8"))
        {
            decode <- fromJSON(mydata)
            message <- decode$exception
            stop (paste("HTTP request was unsuccessful. Status code = ", status, ", Error message = ", message, sep=""))
        } else
        {
            stop (paste("HTTP request was unsuccessful. Status code = ", status, ", Error message = ", message, sep=""))
        }
    }

    mydata
}