\name{labkey.pipeline.getFileStatus}
\alias{labkey.pipeline.getFileStatus}
\title{Gets the protocol file status for a pipeline}
\description{
 Gets the status of analysis using a particular protocol for a particular pipeline.
}
\usage{
labkey.pipeline.getFileStatus(baseUrl=NULL, folderPath,
        taskId, protocolName, path, files)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl} for the LabKey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{taskId}{a string identifier for the pipeline}
  \item{protocolName}{a string name of the analysis protocol}
  \item{path}{a string for the relative path from the folder's pipeline root}
  \item{files}{a list of names of the files within the subdirectory described
    by the path property}
}
\value{
The response will contain a list of file status objects, i.e. files, each of which
will have the following properties:
     \itemize{
  	      \item{ "name": name of the file }
  	      \item{ "status": status of the file }
	}
The response will also include the name of the action that would be performed on
the files if the user initiated processing, i.e. submitType.
}

\author{Cory Nathe}
\seealso{
\code{\link{labkey.pipeline.getPipelineContainer}},
\code{\link{labkey.pipeline.getProtocols}},
\code{\link{labkey.pipeline.startAnalysis}}
}
\examples{
\dontrun{

labkey.pipeline.getFileStatus(
    baseUrl="http://labkey/",
    folderPath="home",
    taskId = "pipelinetest:pipeline:r-copy",
    path = "r-copy",
    protocolName = "Test protocol name",
    files = list("sample.txt", "result.txt")
)

}
}
\keyword{IO}
