\name{labkey.webdav.downloadFolder}
\alias{labkey.webdav.downloadFolder}
\title{Recursively download a folder via WebDAV}
\description{
This will recursively download a folder from a LabKey Server using WebDAV.
}
\usage{
labkey.webdav.downloadFolder(
    localDir,
    baseUrl=NULL, 
    folderPath,
    remoteFilePath,
    overwriteFiles=TRUE,
    mergeFolders=FALSE,
    fileSet='@files'
    )
}
\arguments{
  \item{localDir}{the local filepath where this file will be saved. If this directory exists, the contents of the remote directory will be downloaded here. If not, this local folder will be created; however, this will fail unless the parent folder of localDir exists (similar behavior as linux mv or cp) }
  \item{baseUrl}{a string specifying the \code{baseUrl} for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{remoteFilePath}{the path of this folder on the remote server, relative to the folder root. }
  \item{overwriteFiles}{(optional) if true, any pre-existing file at this location will be overwritten.  Defaults to TRUE }
  \item{mergeFolders}{(optional) if false, any pre-existing local folders in the target location will be deleted if there is an incoming folder of the same name. If true, these existing folders will be left alone, and remote files downloaded into them.  Existing file conflicts will be handled based on the overwriteFiles parameter.  Defaults to TRUE }
  \item{fileSet}{(optional) the name of file server fileSet, which is typically "@files" (the default value for this argument).  In some cases this might be "@pipeline" or "@fileset". }
}
\details{
This will recursively download a folder from a LabKey Server using WebDAV.  This is essentially a wrapper that recursively calls
labkey.webdav.get to download all files in the remote folder.
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

labkey.webdav.downloadFolder(baseUrl="http://labkey/",
    folderPath="home",
    remoteFilePath="folder1",
    localDir="destFolder",
    overwrite=TRUE
)

}
}
\keyword{IO}
