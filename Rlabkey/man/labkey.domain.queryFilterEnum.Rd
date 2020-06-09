\name{labkey.domain.queryFilterEnum}
\alias{labkey.domain.queryFilterEnum}
\title{Provide comparator access}
\description{
Get a list specifying permitted validator comparators.
}
\usage{
labkey.domain.queryFilterEnum()
}
\details{
This function can be used to return a list specifying the set of permitted validator operators, using names to map
conventional terms to the expressions used by LabKey filter URL formats. The values are intended to be used in
conjunction with conditional formats or property validators.
}
\value{
A named list of strings.
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
\code{\link{labkey.domain.queryFilter}}
}
\examples{
\donttest{

library(Rlabkey)

qf <- labkey.domain.queryFilterEnum()

# Example of available comparators
comparator1 <- qf$EQUAL
comparator2 <- qf$GREATER_THAN
comparator3 <- qf$DATE_LESS_THAN_OR_EQUAL
comparator4 <- qf$STARTS_WITH
comparator5 <- qf$CONTAINS_ONE_OF

}
}
\keyword{IO}
