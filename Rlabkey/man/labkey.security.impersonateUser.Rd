\name{labkey.security.impersonateUser}
\alias{labkey.security.impersonateUser}
\title{Start impersonating a user}
\description{
For site-admins or project-admins only, start impersonating a user based on the
userId or email address.
}
\usage{
labkey.security.impersonateUser(baseUrl=NULL, folderPath,
    userId=NULL, email=NULL)
}
\arguments{
  \item{baseUrl}{A string specifying the \code{baseUrl} for the LabKey server.}
  \item{folderPath}{A string specifying the \code{folderPath} in which to impersonate the user.}
  \item{userId}{The id of the user to be impersonated. Either this or email is required.}
  \item{email}{The email of the user to be impersonated. Either this or userID is required.}
}
\details{
Admins may impersonate other users to perform actions on their behalf.
Site admins may impersonate any user in any project. Project admins must
execute this command in a project in which they have admin permission
and may impersonate only users that have access to the project.

To finish an impersonation session use
\code{\link{labkey.security.stopImpersonating}}.
}
\value{
Returns a success message based on a call to \code{\link{labkey.whoAmI}}.
}

\author{Cory Nathe}
\seealso{
\code{\link{labkey.whoAmI}},
\code{\link{labkey.security.stopImpersonating}}
}
\examples{
\dontrun{

library(Rlabkey)

labkey.security.impersonateUser(baseUrl="http://labkey/", folderPath = "/home",
    email = "reader@localhost.test"
)

}
}
\keyword{IO}
