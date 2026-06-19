\name{labkey.storage.update}
\alias{labkey.storage.update}
\title{Update a LabKey Freezer Manager storage item}
\description{
Update an existing LabKey Freezer Manager storage item to change its properties or location within the storage hierarchy.
Storage items can be of the following types: Physical Location, Freezer, Primary Storage, Shelf, Rack, Canister,
Storage Unit Type, or Terminal Storage Location.
}
\usage{
labkey.storage.update(baseUrl=NULL, folderPath, type, props, auditUserComment=NULL)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the LabKey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{type}{a string specifying the type of storage item to update }
  \item{props}{a list properties for the storage item (i.e. name, description, etc.), must include the RowId primary key }
  \item{auditUserComment}{optional reason text that will be attached to the audit log record for this storage change. }
}
\value{
A list containing a data element with the property values for the updated storage item.
}

\author{Cory Nathe}
\seealso{
\code{\link{labkey.storage.create}},
\code{\link{labkey.storage.delete}}
}
\examples{
\dontrun{

library(Rlabkey)
## create a storage unit type and then update it to change some properties

plateType = labkey.storage.create(
    baseUrl="http://labkey/",
    folderPath="home",
    type="Storage Unit Type",
    props=list(name="Test 8X12 Well Plate", unitType="Plate", rows=8, cols=12 )
)

plateType = labkey.storage.update(
    baseUrl="http://labkey/",
    folderPath="home",
    type="Storage Unit Type",
    props=list(rowId=plateType$data$rowId, positionFormat="NumAlpha", positionOrder="ColumnRow" )
)

## supply an auditUserComment ("reason for change") that will be attached to the
## audit log record for this update
plateType = labkey.storage.update(
    baseUrl="http://labkey/",
    folderPath="home",
    type="Storage Unit Type",
    props=list(rowId=plateType$data$rowId, description="Switched to AlphaNum positions"),
    auditUserComment="Switching to AlphaNum positioning per lab SOP-42"
)
}
}
\keyword{IO}
