\name{labkey.storage.create}
\alias{labkey.storage.create}
\title{Create a new LabKey Freezer Manager storage item}
\description{
Create a new LabKey Freezer Manager storage item that can be used in the creation of a storage hierarchy.
Storage items can be of the following types: Physical Location, Freezer, Primary Storage, Shelf, Rack, Canister,
Storage Unit Type, or Terminal Storage Location.
}
\usage{
labkey.storage.create(baseUrl=NULL, folderPath, type, props)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the LabKey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{type}{a string specifying the type of storage item to create }
  \item{props}{a list properties for the storage item (i.e. name, description, etc.) }
}
\value{
A list containing a data element with the property values for the newly created storage item.
}

\author{Cory Nathe}
\seealso{
\code{\link{labkey.storage.update}},
\code{\link{labkey.storage.delete}}
}
\examples{
\dontrun{

library(Rlabkey)
## create a storage Freezer with a Shelf and 2 Plates on that Shelf

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
}
}
\keyword{IO}
