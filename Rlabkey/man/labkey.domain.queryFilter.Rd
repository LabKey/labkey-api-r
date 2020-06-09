\name{labkey.domain.queryFilter}
\alias{labkey.domain.queryFilter}
\title{Create a query filter}
\description{
Create a query filter string.
}
\usage{
labkey.domain.queryFilter(value, filterType, additionalValue=NULL, additionalFilter=NULL)
}
\arguments{
  \item{value}{a string specifying a comparand}
  \item{filterType}{a string specifying a permitted relational operator}
  \item{additionalValue}{a string specifying a second comparand}
  \item{additionalFilter}{a string specifying a second relational operator}

}
\details{
This function can be used to as a convenience wrapper to construct a query filter. Two relational expressions may be
formed, one with the first two parameters (for instance, parameter values '50' and 'eq' for value and filter
respectively would create a condition of 'equals 50') and the second with the remaining two optional parameters. If both
conditions are created, they are conjunct with a logical AND, and a value would have to pass both conditions to clear
the filter.
This function can be used in conjunction with \code{labkey.domain.queryFilterEnum} for easy access to the set of
permitted relational operators.
}
\value{
The string specifying a query filter in LabKey filter URL format.
}

\author{Rosaline Pyktel}
\seealso{
\code{\link{labkey.domain.get}},
\code{\link{labkey.domain.create}},
\code{\link{labkey.domain.createDesign}},
\code{\link{labkey.domain.inferFields}},
\code{\link{labkey.domain.save}},
\code{\link{labkey.domain.drop}},
\code{\link{labkey.domain.conditionalFormat}},
\code{\link{labkey.domain.queryFilterEnum}}
}
\examples{
\donttest{

library(Rlabkey)

qf <- labkey.domain.queryFilterEnum()

# Filters for values equal to 750
qf1 <- labkey.domain.queryFilter(750, qf$EQUAL)
# Filters for values greater than 500, but less than 1000
qf2 <- labkey.domain.queryFilter(500, qf$GREATER_THAN, 1000, qf$LESS_THAN)

}
}
\keyword{IO}
