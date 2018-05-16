\name{labkey.createDomainDesign}
\alias{labkey.createDomainDesign}
\title{Helper function to create a domain design data structure}
\description{
Create a domain design data structure which can then be used by \code{labkey.createDomain} or \code{labkey.saveDomain}
}
\usage{
labkey.createDomainDesign(name, description, fields)
}
\arguments{
  \item{name}{a string specifying the name of the domain}
  \item{description}{(optional) a string specifying domain description }
  \item{fields}{a list containing the fields of the domain design, this should be in the same format as returned by \code{labkey.inferFields}.}
}
\details{
}
\value{
A list containing elements describing the domain design. Any of the APIs which take a domain design parameter can accept this data structure.
}

\author{Karl Lum}
\seealso{
\code{\link{labkey.getDomain}},
\code{\link{labkey.createDomain}},
\code{\link{labkey.inferFields}},
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

}
}
\keyword{IO}
