\name{labkey.webdav.downloadFolder}
\alias{labkey.webdav.downloadFolder}
\title{Recursively download a folder via WebDAV}
\description{
This will recursively download a folder from a LabKey Server using WebDAV.
}
\usage{
labkey.webdav.downloadFolder(localDir='downloadFolder',
    baseUrl=NULL, 
    folderPath='home',
    remoteFilePath='folder1',
    overwrite=TRUE)
}
\arguments{
  \item{localDir}{the local filepath where this file will be saved }
  \item{baseUrl}{a string specifying the \code{baseUrl} for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{fileSet}{(optional) the name of file server fileSet, which is typically "@files" (the default value for this argument). In some cases this might be "@pipeline" or "@fileset". }
  \item{remoteFilePath}{the path of this folder on the remote server, relative to the folder root. }
  \item{overwrite}{(optional) if true, any pre-existing file at this location will be overwritten.  Defaults to TRUE }
}
\details{
This will recursively download a folder from a LabKey Server using WebDAV.  This is essentially a wrapper that recursively calls labkey.webdav.get to download all files in the remote folder.
}
\value{
TRUE or FALSE, depending on if this folder was successfully downloaded
}

\author{Ben Bimber, Ph.D.}
\seealso{
\code{\link{labkey.webdav.get}},
\code{\link{labkey.webdav.put}},
\code{\link{labkey.webdav.mkDir}},
\code{\link{labkey.webdav.mkDirs}},
\code{\link{labkey.webdav.pathExists}},
\code{\link{labkey.webdav.listDir}},
\code{\link{labkey.webdav.delete}}
}
\examples{
\donttest{
## download folder from a LabKey Server
library(Rlabkey)

labkey.webdav.downloadFolder(baseUrl="http://labkey/", folderPath="home", remoteFilePath="folder1", localDir="destFolder", overwrite=TRUE)

}
}
\keyword{IO}
