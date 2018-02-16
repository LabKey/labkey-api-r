##
#  Copyright (c) 2016-2017 LabKey Corporation
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
# For now, just apiKey and baseUrl. TODO: Add email, password, folderPath, and maybe curlOptions & lkOptions
labkey.setDefaults <- function(apiKey="", baseUrl="")
{
    if (apiKey != "")
        .lkdefaults[["apiKey"]] = apiKey;
    if (baseUrl != "")
        .lkdefaults[["baseUrl"]] = baseUrl;
    # for backward compatibility, clear defaults if setDefaults() is called with NO arguments
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
    if (!is.null(baseUrl) && baseUrl != "")
    {
        # set the baseUrl if unset
        if (is.null(.lkdefaults[["baseUrl"]]))
        {
            .lkdefaults[["baseUrl"]] = baseUrl
        }
        return (baseUrl)
    }
    else
    {
        return (.lkdefaults[["baseUrl"]])
    }
}

## helper to retrieve and cache the CSRF token
labkey.getCSRF <- function()
{
    if (is.null(.lkdefaults[["csrf"]]))
    {
        urlBase <- labkey.getBaseUrl()
        if (!is.null(urlBase))
        {
            if (substr(urlBase, nchar(urlBase), nchar(urlBase))!="/")
            {
                urlBase <- paste(urlBase,"/",sep="")
            }
            myUrl <- paste(urlBase, "login/", "whoAmI.view", sep="")
            response <- GET(url=myUrl, config=labkey.getRequestOptions())
            r <- processResponse(response, haltOnError=FALSE)
            json <- fromJSON(r)
            if (!is.null(json$CSRF))
            {
                .lkdefaults[["csrf"]] = json$CSRF
            }
        }
    }
    return (.lkdefaults[["csrf"]])
}

labkey.getRequestOptions <- function(method='GET')
{
    ## Set options
    headerFields <- c()
    if (method == "POST")
        headerFields <- c('Content-Type'="application/json;charset=utf-8")

    options <- labkey.curlOptions()

    ## Support user-settable options for debugging and setting proxies etc
    if(exists(".lksession"))
    {
        userOpt <- .lksession[["curlOptions"]]
        if (!is.null(userOpt))
            options <- c(options, config(userOpt))
    }

    clist <- ifcookie()
    if(clist$Cvalue==1)
    {
        # don't use the httr wrapper because it URL encodes the cookie value
        cook <- config(cookie = paste(clist$Cname, "=", clist$Ccont, sep=""))
        options <- c(options, cook)
    }
    else
    {
        if (method == "GET")
            options <- c(options, config(httpauth=1L))
        apikey <- ifApiKey();
        if (!is.null(apikey))
        {
            headerFields <- c(headerFields, apikey=apikey)
        }
        else
        {
            options <- c(options, config(netrc=1))
        }
    }

    if (method == "POST")
    {
        ## CSRF
        csrf <- labkey.getCSRF()
        if (!is.null(csrf))
            headerFields <- c(headerFields, "X-LABKEY-CSRF" = csrf)
    }
    o <- c(options, add_headers(headerFields))
    return (c(options, add_headers(headerFields)))
}

## Executes an HTTP GET against the supplied URL, with standard handling for session, api key, status codes and error messages.
labkey.get <- function(myurl)
{
    ## HTTP GET
    options <- labkey.getRequestOptions(method="GET")
    response <- GET(url=myurl, config=options)
    processResponse(response)
}

## Executes an HTTP POST of pbody against the supplied URL, with standard handling for session, api key, status codes and error messages.
labkey.post <- function(myurl, pbody)
{
    ## HTTP POST form
    options <- labkey.getRequestOptions(method="POST")
    response <- POST(url=myurl, config=options, body=pbody)
    processResponse(response)
}

processResponse <- function(response, haltOnError=TRUE)
{
    ## Error checking, decode data and return
    status_code <- response$status_code

    status <- http_status(response)
    if(status_code==500 | status_code >= 400)
    {
        ## pull out the error message if possible
        error <- content(response, type = "application/json")
        message = status$message
        if (!is.null(error$exception))
        {
            message <- error$exception
        }
        if (haltOnError)
            stop (paste("HTTP request was unsuccessful. Status code = ", status_code, ", Error message = ", message, sep=""))
    }
    content(response, "text")
}
