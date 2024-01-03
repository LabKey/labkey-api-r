\name{labkey.moveRows}
\alias{labkey.moveRows}
\title{Move rows of data from a LabKey database}
\description{
Specify rows of data to be moved from the LabKey Server
}
\usage{
labkey.moveRows(baseUrl, folderPath, targetFolderPath,
    schemaName, queryName, toMove, options=NULL)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for LabKey server}
  \item{folderPath}{a string specifying the \code{folderPath} for the source container of the rows}
  \item{targetFolderPath}{a string specifying the \code{targetFolderPath} where the rows should be moved}
  \item{schemaName}{a string specifying the \code{schemaName} for the query}
  \item{queryName}{a string specifying the \code{queryName}}
  \item{toMove}{a data frame containing a single column of data containing the data identifiers of the rows to be moved}
  \item{options}{(optional) a list containing options specific to the move action of the query}
}
\details{
Move a set of rows from the source container to a target container for a table. Note that this is not implemented for
all tables.

The list of valid options for each query will vary, but some common examples include:
    \itemize{
        \item{ \code{auditBehavior (string)} : Can be used to override the audit behavior for the table the query is acting on.
            The set of types include: NONE, SUMMARY, and DETAILED.}
        \item{ \code{auditUserComment (string)} : Can be used to provide a comment from the user that will be attached to
            certain detailed audit log records.}
    }
}
\value{
A list is returned with named categories of \bold{command}, \bold{rowsAffected}, \bold{schemaName}, \bold{queryName},
\bold{containerPath} and \bold{updateCounts}.
The \bold{containerPath} will be the target container path where the rows were moved.
The \bold{rowsAffected} property indicates the number of rows affected by the API action. This will typically be the same
number as passed in the request.
The \bold{updateCounts} property is a list of the number of items moved for various related items.
}
\author{Cory Nathe}
\seealso{
\code{\link{labkey.deleteRows}}, \cr
\code{\link{labkey.importRows}}, \cr
\code{\link{labkey.importRows}}, \cr
\code{\link{labkey.updateRows}},
}

\examples{
\dontrun{

## Note that users must have the necessary permissions in the LabKey Server
## to be able to modify data through the use of these functions
# library(Rlabkey)

newrow <- data.frame(
	DisplayFld="Inserted from R"
	, IntFld= 98
	, DateTimeFld = "03/01/2010"
	, stringsAsFactors=FALSE)

insertedRow <- labkey.insertRows("http://localhost:8080/labkey",
    folderPath="/apisamples", schemaName="samples",
    queryName="Blood", toInsert=newrow)
newRowId <- insertedRow$rows[[1]]$RowId

result <- labkey.moveRows(baseUrl="http://localhost:8080/labkey",
    folderPath="/apisamples", folderPath="/apisamples/subA", schemaName="samples",
    queryName="Blood",  toMove=data.frame(RowId=c(newRowId)),
    options = list(auditUserComment="testing comment from API call", auditBehavior="DETAILED"))
result

}
}
\keyword{IO}
