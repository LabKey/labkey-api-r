\name{labkey.security.stopImpersonating}
\alias{labkey.security.stopImpersonating}
\title{Stop impersonating a user}
\description{
Stop impersonating a user while keeping the original user logged in.
}
\usage{
labkey.security.stopImpersonating(baseUrl=NULL)
}
\arguments{
  \item{baseUrl}{A string specifying the \code{baseUrl} for the LabKey server.}
}
\details{
If you are currently impersonating a user in this session, you can use this
function to stop the impersonation and return back to the original user logged in.

To start an impersonation session use
\code{\link{labkey.security.impersonateUser}}.
}
\value{
Returns a success message based on a call to \code{\link{labkey.whoAmI}}.
}

\author{Cory Nathe}
\seealso{
\code{\link{labkey.whoAmI}},
\code{\link{labkey.security.impersonateUser}}
}
\examples{
\dontrun{

library(Rlabkey)

labkey.security.stopImpersonating(baseUrl="http://labkey/")

}
}
\keyword{IO}
