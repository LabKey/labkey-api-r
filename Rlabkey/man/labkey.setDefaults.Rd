\name{labkey.setDefaults}
\alias{labkey.setDefaults}
\title{Set the default parameters used for all http or https requests}
\description{
Use this function to set the default baseUrl and authentication parameters as package environment variables to be used
for all http or https requests. You can also use labkey.setDefaults() without any parameters to reset/clear
these settings.}
\usage{
labkey.setDefaults(apiKey="", baseUrl="", email="", password="")
}
\arguments{
  \item{apiKey}{api or session key from your server}
  \item{baseUrl}{server location including context path, if any. e.g. https://www.labkey.org/}
  \item{email}{user email address}
  \item{password}{user password}
}

\details{
An API key can be used to authorize Rlabkey functions that access secure content on LabKey Server. Using an API key
avoids copying and storing credentials on the client machine. An API key can be revoked and set to expire. It also
acts as a credential for those who sign in using a single sign-on authentication mechanism such as CAS or SAML.\cr

A site administrator must first enable the use of API keys on that LabKey Server. Once enabled, any logged in user can
generate an API key by clicking their display name (upper right) and selecting "External Tool Access". The API Key page
creates and displays keys that can be copied and pasted into a labkey.setDefaults() statement to give an Rlabkey
session the permissions of the corresponding user.\cr

If an API key is not provided, you can also use this function for basic authentication via email and password. Note
that both email and password must be set via a labkey.setDefaults() call. If an API key is also set, that will be given
preference and the email/password will not be used for authentication.\cr

On servers that enable them, a session key can be used in place of an API key. A session key ties all Rlabkey access
to a user's current browser session, which means the code runs in the same context as the browser (e.g. same user,
same authorizations, same declared terms of use and PHI level, same impersonation state, etc.). Session keys can be
useful in certain compliance scenarios.\cr

Once valid credentials are provided to labkey.setDefaults(), subsequent labkey.get or labkey.post API calls will
authenticate using those credentials.\cr
}
\examples{

## Example of setting and clearing email/password, API key, and Session key
# library(Rlabkey)

labkey.setDefaults(email="testing@localhost.test", password="password")

## Functions invoked at this point respect the role assignments and
## other authorizations of the specified user

## A user can create an API key via the LabKey UI and set it as follows:

labkey.setDefaults(apiKey="abcdef0123456789abcdef0123456789")

## Functions invoked at this point respect the role assignments and
## other authorizations of the user who created the API key

## A user can create a session key via the LabKey UI and set it as follows:

labkey.setDefaults(apiKey="0123456789abcdef0123456789abcdef")

## Functions invoked at this point share authorization
## and session information with the user's browser session

labkey.setDefaults() # called without any parameters will reset/clear the environment variables

}
