\name{labkey.inferFields}
\alias{labkey.inferFields}
\title{Infer field metadata from a data frame}
\description{
Generate field information from the specified data frame. The resulting list can be used to create or edit a domain using the labkey.createDomain or labkey.saveDomain APIs.
}
\usage{
labkey.inferFields(baseUrl = NULL, folderPath, fields)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl} for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{fields}{a data frame specifying fields to infer. The data frame must have column names as well as row data to infer the type of the field from.}
}
\details{
}
\value{
The inferred metadata will be returned as a list with an element called : "fields" which contains the list of fields inferred from the data frame.
Each field will contain the list of attributes and values for that field definition.
}

\author{Karl Lum}
\seealso{
\code{\link{labkey.getDomain}},
\code{\link{labkey.createDomain}},
\code{\link{labkey.createDomainDesign}},
\code{\link{labkey.saveDomain}},
\code{\link{labkey.deleteDomain}}
}
\examples{
\donttest{

## create a data frame and infer it's fields
library(Rlabkey)

df <- data.frame(ptid=c(1:3), age = c(10,20,30), sex = c("f", "m", "f"))
fields <- labkey.inferFields(baseUrl="http://labkey/", folderPath="home", df)

}
}
\keyword{IO}
