\name{makeFilter}
\alias{makeFilter}
\title{Builds filters to be used in labkey.selectRows and getRows}
\description{
This function takes inputs of column name, filter value and filter operator and 
returns an array of filters to be used in \code{labkey.selectRows} and \code{getRows}.
}
\usage{
makeFilter(..., asList=FALSE)
}
\arguments{
  \item{...}{Arguments in c("colname","operator","value") form, used to create a filter.}
  \item{asList}{Boolean flag when set to TRUE will format the return value as a list with named elements.}
}
\details{
These filters are applied to the data prior to import into R. The user can specify as many 
filters as desired. The format for specifying a filter is a vector of characters including
the column name, operator and value.
\describe{
\item{colname}{a string specifying the name of the column to be filtered} 
\item{operator}{a string specifying what operator should be used in the filter (see options below)}
\item{value}{an integer or string specifying the value the columns should be filtered on}
}

Operator values:\cr
ARRAY_CONTAINS_ALL\cr
ARRAY_CONTAINS_ANY\cr
ARRAY_CONTAINS_EXACT\cr
ARRAY_CONTAINS_NOT_EXACT\cr
ARRAY_CONTAINS_NONE\cr
ARRAY_ISEMPTY\cr
ARRAY_ISNOTEMPTY\cr
EQUAL\cr
DATE_EQUAL\cr
NOT_EQUAL\cr
DATE_NOT_EQUAL\cr
NOT_EQUAL_OR_MISSING\cr
GREATER_THAN\cr
DATE_GREATER_THAN\cr
LESS_THAN\cr
DATE_LESS_THAN\cr
GREATER_THAN_OR_EQUAL\cr
DATE_GREATER_THAN_OR_EQUAL\cr
LESS_THAN_OR_EQUAL\cr
DATE_LESS_THAN_OR_EQUAL\cr
STARTS_WITH\cr
DOES_NOT_START_WITH\cr
CONTAINS\cr
DOES_NOT_CONTAIN\cr
CONTAINS_ONE_OF\cr
CONTAINS_NONE_OF\cr
IN\cr
NOT_IN\cr
BETWEEN\cr
NOT_BETWEEN\cr
MEMBER_OF\cr
MISSING\cr
NOT_MISSING\cr
MV_INDICATOR\cr
NO_MV_INDICATOR\cr
Q\cr
ONTOLOGY_IN_SUBTREE\cr
ONTOLOGY_NOT_IN_SUBTREE\cr
EXP_CHILD_OF\cr
EXP_PARENT_OF\cr
EXP_LINEAGE_OF\cr

When using the MISSING, NOT_MISSING, MV_INDICATOR, NO_MV_INDICATOR, ARRAY_ISEMPTY, or ARRAY_ISNOTEMPTY operators, an empty string should be supplied as the value.
See example below.
}


\value{
The function returns either a single string or an array of strings to be use in the
\code{colFilter} argument of the \code{labkey.selectRows} function. By default, this function will return an array
of LabKey filter parameters/values that are URL encoded and can be directly applied to a request URL. If the asList argument
is set to TRUE, the function will return a list with named elements where the name is the parameter name and the element
value is the filter value. This format can be useful when needing to build URLs that need to be combined with other
parameters or can be converted directly to JSON posted parameters.
}
\references{http://www.omegahat.net/RCurl/, \cr
https://www.labkey.org/home/project-begin.view}
\author{Valerie Obenchain}
\seealso{\code{\link{labkey.selectRows}}}
\examples{

# library(Rlabkey)

## Two filters, ANDed together
makeFilter(c("TextFld","CONTAINS","h"),
    c("BooleanFld","EQUAL","TRUE"))

## Using "in" operator:
makeFilter(c("RowId","IN","2;3;6"))

## Using "missing" operator:
makeFilter(c("IntFld","MISSING",""))

}
\keyword{file}
