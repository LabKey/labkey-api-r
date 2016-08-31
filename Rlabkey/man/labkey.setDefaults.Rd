\name{labkey.setDefaults}
\alias{labkey.setDefaults}
\title{Set the default parameters used for all http or https requests}
\description{
Use this function to set the "apiKey" and "baseUrl" package environment variables to be used for all http or https requests.
You can also use labkey.setDefaults() without any parameters to reset/clear the settings.}
\usage{
labkey.setDefaults(apiKey='value')
labkey.setDefaults(baseUrl='value')
}
\arguments{
  \item{apiKey}{session key from your server}
  \item{baseUrl}{server location including context path, if any. e.g. https://www.labkey.org/}
}

\details{
Note: Support for API keys was added in LabKey Server release 16.2; they are not supported in 16.1 or earlier.

An API key can be used to authorize Rlabkey functions that access protected content on LabKey Server. Using an API key
avoids copying and storing credentials on the client machine. Also, all Rlabkey access is tied to the current browser
session, which means the code runs in the same context as the browser (e.g., same user, same authorizations, same
declared terms of use and PHI level, same impersonation state, etc.).\cr

A site administrator must first enable the use of API keys on that LabKey Server. Once enabled, any logged in user can
generate an API key by clicking their display name (upper right) and selecting "API Key". The API Key page displays a
key that can be copied and pasted into a labkey.setDefaults() statement to tie an Rlabkey session to the authorization
and session information already set in the browser.\cr
}
\examples{
\dontrun{

## Example of setting and clearing an API key.
# library(Rlabkey)

labkey.setDefaults(apiKey="session|abcdef0123456789abcdef0123456789")

## Functions invoked at this point share authorization
## and session information with the browser session

labkey.setDefaults()
or
labkey.setDefaults(apiKey=NULL)

## Functions no longer share authorization and session
## information with the browser session

}
}
