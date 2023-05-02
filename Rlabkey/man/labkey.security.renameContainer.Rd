\name{labkey.security.renameContainer}
\alias{labkey.security.renameContainer}
\title{Rename an existing container at the given container path}
\description{
Renames an existing container at the given container path.
This action allows for updating the container name, title, or both.
}
\usage{
labkey.security.renameContainer(baseUrl=NULL, folderPath,
    name=NULL, title=NULL, addAlias=TRUE)
}
\arguments{
  \item{baseUrl}{A string specifying the \code{baseUrl} for the labkey server.}
  \item{folderPath}{A string specifying the \code{folderPath} to be renamed. Additionally, the container entity id is also valid.}
  \item{name}{The new container name. If not specified, the container name will not be changed.}
  \item{title}{The new container title. If not specified, the container name will be used.}
  \item{addAlias}{Add alias of current container path for the current container name (defaults to true).}
}
\details{
This function renames an existing container at the given container path on the LabKey server.
A new container name and/or title must be specified. If a new name is provided but not a title, the name will also
be set as the container title.
}
\value{
Returns a success message for the container rename action.
}

\author{Cory Nathe}
\seealso{
\code{\link{labkey.getFolders}},
\code{\link{labkey.security.getContainers}},
\code{\link{labkey.security.createContainer}},
\code{\link{labkey.security.deleteContainer}}
\code{\link{labkey.security.moveContainer}}
}
\examples{
\dontrun{

library(Rlabkey)

labkey.security.renameContainer(baseUrl="http://labkey/", folderPath = "/home/OriginalFolder",
    name = "NewFolderName", title = "New Folder Title", addAlias = TRUE
)

}
}
\keyword{IO}
