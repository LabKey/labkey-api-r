\name{labkey.deleteDomain}
\alias{labkey.deleteDomain}
\title{Delete a LabKey domain}
\description{
Delete an existing domain.
}
\usage{
labkey.deleteDomain(baseUrl=NULL, folderPath, schemaName, queryName)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{schemaName}{a string specifying the name of the schema of the domain }
  \item{queryName}{a string specifying the query name }
}
\details{
}

\author{Karl Lum}
\seealso{
\code{\link{labkey.getDomain}},
\code{\link{labkey.createDomain}},
\code{\link{labkey.createDomainDesign}},
\code{\link{labkey.saveDomain}},
\code{\link{labkey.inferFields}}
}
\examples{
\donttest{

## delete an existing domain
library(Rlabkey)

labkey.deleteDomain(baseUrl="http://labkey/", folderPath="home", schemaName="lists", queryName="test list")
}
}
\keyword{IO}
