\name{labkey.whoAmI}
\alias{labkey.whoAmI}
\title{Call the whoami API}
\description{
Call the whoami API to get information about the current LabKey user.
}
\usage{
labkey.whoAmI(baseUrl=NULL)
}
\arguments{
  \item{baseUrl}{A string specifying the \code{baseUrl} for the LabKey server.}
}
\value{
Returns information about the logged in user including: displayName, id, email,
and whether or not the user is impersonated.
}

\author{Cory Nathe}
\seealso{
\code{\link{labkey.security.impersonateUser}},
\code{\link{labkey.security.stopImpersonating}}
}
\examples{
\donttest{

library(Rlabkey)

labkey.whoAmI(baseUrl="http://labkey/")

}
}
\keyword{IO}
