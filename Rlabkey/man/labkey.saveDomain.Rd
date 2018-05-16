\name{labkey.saveDomain}
\alias{labkey.saveDomain}
\title{Updates an existing LabKey domain}
\description{
Modify an existing domain with the specified domain design.
}
\usage{
labkey.saveDomain(baseUrl=NULL, folderPath, schemaName, queryName, domainDesign)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{schemaName}{a string specifying the name of the schema of the domain }
  \item{queryName}{a string specifying the query name }
  \item{domainDesign}{a list data structure with the domain design to update to }
}
\details{
}
\value{
A list containing elements describing the domain after the update. The structure is the same as a domain design created by \code{labkey.createDomainDesign}
}

\author{Karl Lum}
\seealso{
\code{\link{labkey.getDomain}},
\code{\link{labkey.createDomain}},
\code{\link{labkey.createDomainDesign}},
\code{\link{labkey.inferFields}},
\code{\link{labkey.deleteDomain}}
}
\examples{
\donttest{

## retrieve an existing domain
library(Rlabkey)

labkey.getDomain(baseUrl="http://labkey/", folderPath="home", schemaName="lists", queryName="test list")
}
}
\keyword{IO}
