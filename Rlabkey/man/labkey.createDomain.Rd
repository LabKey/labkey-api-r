\name{labkey.createDomain}
\alias{labkey.createDomain}
\title{Create a new LabKey domain}
\description{
Create a domain of the type specified by the domainKind and the domainDesign. A LabKey domain represents a table in a specific schema.
}
\usage{
labkey.createDomain(baseUrl=NULL, folderPath, domainKind, domainDesign, options)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{domainKind}{a string specifying the type of domain to create }
  \item{domainDesign}{a list containing the domain design to create }
  \item{options}{a list containing options specific to the domain kind }
}
\details{
}
\value{
A list containing elements describing the newly created domain.
}

\author{Karl Lum}
\seealso{
\code{\link{labkey.getDomain}},
\code{\link{labkey.inferFields}},
\code{\link{labkey.createDomainDesign}},
\code{\link{labkey.saveDomain}},
\code{\link{labkey.deleteDomain}}
}
\examples{
\donttest{

## create a data frame and infer it's fields, then create a domain design from it
library(Rlabkey)

df <- data.frame(ptid=c(1:3), age = c(10,20,30), sex = c("f", "m", "f"))
fields <- labkey.inferFields(baseUrl="http://labkey/", folderPath="home", df)
dd <- labkey.createDomainDesign(name="test list", fields=fields)

## create a new list with an integer key field
labkey.createDomain(baseUrl="http://labkey/", folderPath="home", domainKind="IntList", domainDesign=dd, options=list(keyName = "ptid"))

}
}
\keyword{IO}
