\name{labkey.query.import}
\alias{labkey.query.import}
\title{Bulk import an R data frame into a LabKey Server table using file import.}
\description{
Bulk import an R data frame into a LabKey Server table using file import.
}
\usage{
labkey.query.import(baseUrl, folderPath,
    schemaName, queryName, toImport, options = NULL)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{schemaName}{a string specifying the \code{schemaName} for the query}
  \item{queryName}{a string specifying the \code{queryName}}
  \item{toImport}{a data frame containing rows of data to be imported}
  \item{options}{(optional) a list containing options specific to the import
  action of the query}
}
\details{
This command mimics the "Import bulk data" option that you see in the LabKey server UI
for a table/query. It takes the passed in \code{toImport} data frame and writes it to a
temp file to be posted to the import action for the given LabKey query. It is very similar
to the \code{labkey.importRows} command but will be much more performant.
\cr \cr
Multiple rows of data can be imported in bulk using the \code{toImport} data frame.
The names of the data in the data frame must be the column names from the
LabKey Server.
\cr \cr
LabKey data types support different import options. The list of valid options for each
query will vary, but some common examples include:
    \itemize{
        \item{ \code{insertOption (string)} : Whether the import action should be done as an insert, creating new
            rows for each provided row of the data frame, or a merge. When merging during import, any data you provide
            for the rows representing records that already exist will replace the previous values.
            Note that when updating an existing record, you only need to provide the columns you wish to update,
            existing data for other columns will be left as is. Available options are "INSERT" and "MERGE".
            Defaults to "INSERT".}
        \item{ \code{auditBehavior (string)} : Set the level of auditing details for this import action.
            Available options are "SUMMARY" and "DETAILED". SUMMARY - Audit log reflects that a change was made,
            but does not mention the nature of the change. DETAILED - Provides full details on what change was made,
            including values before and after the change. Defaults to the setting as specified by the LabKey query.}
        \item{ \code{importLookupByAlternateKey (boolean)} : Allows lookup target rows to be resolved by values
            rather than the target's primary key. This option will only be available for lookups that are
            configured with unique column information. Defaults to FALSE.}
    }
}

\value{
A list is returned with the row count for the number of affected rows. If options are provided,
additional details may be included in the response object related to those options.
}
\author{Cory Nathe}
\seealso{
\code{\link{labkey.insertRows}}, \code{\link{labkey.updateRows}}, \code{\link{labkey.importRows}}
}
\examples{
\dontrun{

## Note that users must have the necessary permissions in the database
## to be able to modify data through the use of these functions
# library(Rlabkey)

df <- data.frame(
    name=c("test1","test2","test3"),
    customInt=c(1:3),
    customString=c("aaa", "bbb", "ccc")
)

importedInfo <- labkey.query.import(
    "http://localhost:8080/labkey",
    folderPath="/apisamples", schemaName="samples", queryName="SampleType1",
    toImport=df, options=list(insertOption = "MERGE", auditBehavior = "DETAILED")
)

importedInfo$rowCount

}
}
\keyword{IO}