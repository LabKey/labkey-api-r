\name{labkey.storage.delete}
\alias{labkey.storage.delete}
\title{Delete a LabKey Freezer Manager storage item}
\description{
Delete an existing LabKey Freezer Manager storage item. Note that deletion of freezers, primary storage, or locations
within the storage hierarchy will cascade the delete down the hierarchy to remove child locations and terminal storage
locations. Samples in the deleted storage location(s) will not be deleted but will be removed from storage.
Storage items can be of the following types: Physical Location, Freezer, Primary Storage, Shelf, Rack, Canister,
Storage Unit Type, or Terminal Storage Location.
}
\usage{
labkey.storage.delete(baseUrl=NULL, folderPath, type, rowId)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the LabKey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{type}{a string specifying the type of storage item to delete }
  \item{rowId}{the primary key of the storage item to delete }
}
\value{
A list containing a data element with the property values for the deleted storage item.
}

\author{Cory Nathe}
\seealso{
\code{\link{labkey.storage.create}},
\code{\link{labkey.storage.update}}
}
\examples{
\dontrun{

library(Rlabkey)
## delete a freezer and its child locations and terminal storage locations

freezer <- labkey.storage.create(
    baseUrl="http://labkey/",
    folderPath="home",
    type="Freezer",
    props=list(name="Test Freezer", description="My example storage freezer")
)

shelf = labkey.storage.create(
    baseUrl="http://labkey/",
    folderPath="home",
    type="Shelf",
    props=list(name="Test Shelf", locationId=freezer$data$rowId )
)

plateType = labkey.storage.create(
    baseUrl="http://labkey/",
    folderPath="home",
    type="Storage Unit Type",
    props=list(name="Test 8X12 Well Plate", unitType="Plate", rows=8, cols=12 )
)

plate1 = labkey.storage.create(
    baseUrl="http://labkey/",
    folderPath="home",
    type="Terminal Storage Location",
    props=list(name="Plate #1", typeId=plateType$data$rowId, locationId=shelf$data$rowId )
)

plate2 = labkey.storage.create(
    baseUrl="http://labkey/",
    folderPath="home",
    type="Terminal Storage Location",
    props=list(name="Plate #2", typeId=plateType$data$rowId, locationId=shelf$data$rowId )
)

# NOTE: this will delete freezer, shelf, plate1 and plate2 but it will not delete
#       the plateType as that is not a part of the freezer hierarchy
freezer <- labkey.storage.delete(
    baseUrl="http://labkey/",
    folderPath="home",
    type="Freezer",
    rowId=freezer$data$rowId
)
}
}
\keyword{IO}
