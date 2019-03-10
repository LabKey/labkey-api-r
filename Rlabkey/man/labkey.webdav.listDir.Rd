\name{labkey.webdav.listDir}
\alias{labkey.webdav.listDir}
\title{List the contents of a LabKey Server folder via WebDAV}
\description{
This will list the contents of a LabKey Server folder using WebDAV.
}
\usage{
json <- labkey.webdav.listDir(
    baseUrl=NULL,
    folderPath='home',
    remoteFilePath='folder1')
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl} for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{fileSet}{(optional) the name of file server fileSet, which is typically "@files" (the default value for this argument). In some cases this might be "@pipeline" or "@fileset". }
  \item{remoteFilePath}{path of the folder on the remote server, relative to the folder root. }
}
\details{
Lists the contents of a folder on a LabKey Server using WebDAV.
}
\value{
A list with each item under this folder.  Each file has the following attributes:


}

\author{Ben Bimber, Ph.D.}
\seealso{
\code{\link{labkey.webdav.get}},
\code{\link{labkey.webdav.put}},
\code{\link{labkey.webdav.mkDir}},
\code{\link{labkey.webdav.mkDirs}},
\code{\link{labkey.webdav.pathExists}},
\code{\link{labkey.webdav.delete}},
\code{\link{labkey.webdav.downloadFolder}}
}
\examples{
\donttest{

library(Rlabkey)

json <- labkey.webdav.listDir(baseUrl="http://labkey/", folderPath="home", remoteFilePath="myFolder")

}
}
\keyword{IO}
