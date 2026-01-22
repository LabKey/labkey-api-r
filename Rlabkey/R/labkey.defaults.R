##
#  Copyright (c) 2016-2018 LabKey Corporation
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
.lkcsrf <- new.env(parent=emptyenv());

# Set the credentials used for all http or https requests. Note that if both apiKey and email/password are provided,
# the apiKey will be given preference in labkey.getRequestOptions().
labkey.setDefaults <- function(apiKey="", baseUrl="", email="", password="")
{
    # with any reset of the defaults, clear the httr session cookies (https://stackoverflow.com/questions/39979393/how-to-remove-cookies-preserved-by-httrget)
    if (!is.null(.lkdefaults$baseUrl)) {
        handle_reset(.lkdefaults$baseUrl)

        # Issue 50592: clear the CSRF token cache as well
        urlBase = labkey.getBaseUrl()
        .lkcsrf[[urlBase]] = NULL
    }

    if (baseUrl != "")
        .lkdefaults$baseUrl = baseUrl;

    if (apiKey != "")
        .lkdefaults$apiKey = apiKey;

    if (email != "" && password != "") {
        .lkdefaults$email = email;
        .lkdefaults$password = password;
    }

    # for backward compatibility, clear defaults if setDefaults() is called with NO arguments
    if (baseUrl == "" && apiKey == "" && email == "" && password == "") {
        if (!is.null(.lkdefaults$baseUrl)) { rm("baseUrl", envir = .lkdefaults) }
        if (!is.null(.lkdefaults$apiKey)) { rm("apiKey", envir = .lkdefaults) }
        if (!is.null(.lkdefaults$email)) { rm("email", envir = .lkdefaults) }
        if (!is.null(.lkdefaults$password)) { rm("password", envir = .lkdefaults) }
    }
}

isPasswordAuth <- function()
{
    return (!is.null(.lkdefaults$password) && .lkdefaults$password != "")
}

ifApiKey <- function()
{
    if (exists("labkey.apiKey", envir = .GlobalEnv)) {
        get("labkey.apiKey", envir = .GlobalEnv)
    } else {
        .lkdefaults$apiKey;
    }
}

labkey.getBaseUrl <- function(baseUrl=NULL)
{
    if (!is.null(baseUrl) && baseUrl != "")
    {
        # set the baseUrl if unset
        if (is.null(.lkdefaults$baseUrl) || (.lkdefaults$baseUrl != baseUrl))
        {
            .lkdefaults$baseUrl = baseUrl
        }
        url <- baseUrl
    }
    else
    {
        url <- .lkdefaults$baseUrl
    }

    if (is.null(url))
        stop (paste("baseUrl is null or has not been set yet."))

    ## convert any backslashes to forward slashes, ensure terminating slash
    url <- gsub("[\\]", "/", url)
    if(substr(url, nchar(url), nchar(url))!="/")
    {
        url <- paste(url,"/",sep="")
    }
    return (url)
}

## helper to encode and normalize the folder path parameter
encodeFolderPath <- function(folderPath=NULL)
{
    if (!is.null(folderPath))
    {
        ## URL encoding of folderPath
        folderPath <- URLencode(folderPath)

        folderPath <- normalizeSlash(folderPath)
    }
    return (folderPath)
}

normalizeSlash <- function(folderPath, leading = T, trailing = T) {
  ## Formatting
  folderPath <- gsub("[\\]", "/", folderPath)
  
  if (trailing) {
    if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/")
      folderPath <- paste(folderPath,"/",sep="")
  } else {
    if(substr(folderPath, nchar(folderPath), nchar(folderPath))=="/")
      folderPath <- substr(folderPath,1, nchar(folderPath)-1)
  }
  
  if (leading) {
    if(substr(folderPath, 1, 1)!="/")
      folderPath <- paste("/",folderPath,sep="")
  } else {
    if(substr(folderPath, 1, 1)=="/")
      folderPath <- substr(folderPath,2, nchar(folderPath))
  }
  
  return(folderPath)
}

labkey.whoAmI <- function(baseUrl=NULL)
{
    baseUrl <- labkey.getBaseUrl(baseUrl)
    myUrl <- paste(baseUrl, "login/", "whoAmI.view", sep="")
    options <- labkey.getRequestOptions()
    verboseOutput("OPTIONS", options)
    response <- GET(url=myUrl, config=options)
    r <- processResponse(response, haltOnError=FALSE)
    return (fromJSON(r, simplifyVector=FALSE, simplifyDataFrame=FALSE))
}

## helper to retrieve and cache the CSRF token
labkey.getCSRF <- function()
{
    urlBase <- labkey.getBaseUrl()
    if (!is.null(urlBase))
    {
        if (is.null(.lkcsrf[[urlBase]]))
        {
            if (substr(urlBase, nchar(urlBase), nchar(urlBase))!="/")
            {
                urlBase <- paste(urlBase,"/",sep="")
            }
            json <- labkey.whoAmI(urlBase)
            if (!is.null(json$CSRF))
            {
                .lkcsrf[[urlBase]] = json$CSRF
            }
        }
        return (.lkcsrf[[urlBase]])
    }
}

labkey.getRequestOptions <- function(method='GET', encoding=NULL)
{
    ## Set options
    headerFields <- c()
    if (method == "POST")
    {
       if (is.null(encoding) || encoding != "multipart")
           headerFields <- c('Content-Type'="application/json;charset=utf-8")

       ## CSRF
       csrf <- labkey.getCSRF()
       if (!is.null(csrf))
           headerFields <- c(headerFields, "X-LABKEY-CSRF" = csrf)
    }

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
        if (!is.null(apikey) && apikey != "") {
            headerFields <- c(headerFields, apikey=apikey)
        }
        else if (isPasswordAuth()) {
            options <- c(options, authenticate(.lkdefaults$email, .lkdefaults$password))
        }
        else {
            options <- c(options, config(netrc=1))
        }
    }

    if (isDebug())
        options <- c(options, verbose(data_in=TRUE, info=TRUE, ssl=TRUE))

    return (c(options, add_headers(headerFields)))
}

## Executes an HTTP GET against the supplied URL, with standard handling for session, api key, status codes and error messages.
labkey.get <- function(myurl)
{
    ## HTTP GET
    options <- labkey.getRequestOptions(method="GET")
    verboseOutput("OPTIONS", options)
    response <- GET(url=myurl, config=options)
    processResponse(response)
}

## Executes an HTTP POST of pbody against the supplied URL, with standard handling for session, api key, status codes and error messages.
labkey.post <- function(myurl, pbody, encoding=NULL, responseType=NULL, haltOnError=TRUE)
{
    ## HTTP POST form
    options <- labkey.getRequestOptions(method="POST", encoding=encoding)
    verboseOutput("OPTIONS", options)
    response <- POST(url=myurl, config=options, body=pbody)
    processResponse(response, responseType = responseType, haltOnError = haltOnError)
}

processResponse <- function(response, haltOnError=TRUE, responseType = NULL)
{
    if(isRequestError(response))
    {
      handleError(response, haltOnError)
    }
    content(response, as = "text", type = responseType)
}

labkey.setDebugMode <- function(debug=FALSE)
{
    .lkdefaults$debug = debug;
}

isDebug <- function()
{
    if (is.null(.lkdefaults$debug)) {
        return (FALSE)
    }
    return (.lkdefaults$debug)
}

labkey.setWafEncoding <- function(wafEncode=TRUE)
{
    .lkdefaults$wafEncode = wafEncode;
}

isWafEncoding <- function()
{
    if (is.null(.lkdefaults$wafEncode)) {
        return (TRUE)
    }
    return (.lkdefaults$wafEncode)
}

isRequestError <- function(response, status_code) 
{
  status_code <- getStatusCode(response)
  
  return(status_code==500 | status_code >= 400)
}

getStatusCode <- function(response) 
{
  ## Error checking, decode data and return
  status_code <- response$status_code
  
  #test for the situations where the header reports 200, but the JSON contains the error:
  if (sum(grepl('application/json', response$headers[['content-type']])) > 0) {
    c <- content(response, type = "application/json")
    if (!is.null(c[['status']])) {
      status_code <- c$status
    }
  }

  return(status_code)  
}

handleError <- function(response, haltOnError) 
{
  status <- http_status(response)
  message = status$message
  
  ## pull out the error message if possible
  error <- content(response, type = "application/json")
  if (!is.null(error$exception))
  {
    message <- error$exception
  }
  if (haltOnError) {
    status_code <- getStatusCode(response)

    # Note: is this request was writing to a file, the error message JSON will be written to that file, so we delete.  
    if (inherits(response$content, 'path')) {
      if (file.exists(response$content)){
        unlink(response$content)  
      } 
    }

    stop (paste("HTTP request was unsuccessful. Status code = ", status_code, ", Error message = ", message, sep=""))
  }
}

verboseOutput <- function(title, content)
{
    if (isDebug()) {
        print(paste("*******************BEGIN ",title,"*******************", sep=""))
        print(content)
        print(paste("*******************END ",title,"*********************", sep=""))
    }
}

# Obfuscates content that's often intercepted by web application firewalls that are scanning for likely
# SQL or script injection. We have a handful of endpoints that intentionally accept SQL or script, so we
# encode the text to avoid tripping alarms. It's a simple BASE64 encoding that obscures the content, and lets the
# WAF scan for and reject malicious content on all other parameters. See Issue 48509.
wafEncode <- function(value)
{
    if (!is.null(value) && is.character(value))
    {
        value <- encodeURIComponent(value)
        value <- base64_enc(value)
        value <- gsub("\n", "", value) # jsonlite:base64_enc concats long strings with \n char
        value <- paste0("/*{{base64/x-www-form-urlencoded/wafText}}*/", value)
    }
    return (value)
}

# URL Encode string.
# NOTE: made to match JavaScript encodeURIComponent()
encodeURIComponent <- function(value)
{
    value <- URLencode(value, reserved = TRUE)
    # The following characters need to be decoded to match server side behavior: !  * ' ( )
    value <- gsub("%21", "!", value)
    value <- gsub("%2A", "*", value)
    value <- gsub("%27", "'", value)
    value <- gsub("%28", "(", value)
    value <- gsub("%29", ")", value)
    return (value)
}

# Construct a LabKey URL (path first format)
labkey.buildURL <- function(baseUrl=NULL, controller, action, folderPath = NULL, parameters = NULL)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    # check required parameters
    if (missing(baseUrl) || missing(controller) || missing(action) || is.null(folderPath))
        stop (paste("A value must be specified for each of baseUrl, controller, action and folderPath."))

    # normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    myUrl <- paste(baseUrl, folderPath, controller, "-", action, sep="")

    if (!is.null(parameters))
    {
        if (!is.list(parameters))
            stop (paste("parameters must be a list data structure."))

        # add the parameters as a query string
        url <- parse_url(myUrl)
        url$query = parameters

        myUrl <- build_url(url)
    }
    return (myUrl)
}