\name{labkey.getDomain}
\alias{labkey.getDomain}
\title{Returns the metadata for an existing LabKey domain}
\description{
Get the data structure for a domain.
}
\usage{
labkey.getDomain(baseUrl=NULL, folderPath, schemaName, queryName)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{schemaName}{a string specifying the name of the schema of the domain }
  \item{queryName}{a string specifying the query name }
}
\details{
}
\value{
A list containing elements describing the domain. The structure is the same as a domain design created by \code{labkey.createDomainDesign}
}

\author{Karl Lum}
\seealso{
\code{\link{labkey.inferFields}},
\code{\link{labkey.createDomain}},
\code{\link{labkey.createDomainDesign}},
\code{\link{labkey.saveDomain}},
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
