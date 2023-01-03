\name{labkey.pipeline.getProtocols}
\alias{labkey.pipeline.getProtocols}
\title{Gets the protocols that have been saved for a particular pipeline}
\description{
 Gets the protocols that have been saved for a particular pipeline.
}
\usage{
labkey.pipeline.getProtocols(baseUrl=NULL, folderPath,
        taskId, path, includeWorkbooks = FALSE)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl} for the LabKey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{taskId}{a string identifier for the pipeline}
  \item{path}{a string for the relative path from the folder's pipeline root}
  \item{includeWorkbooks}{(optional) If true, protocols from workbooks under
        the selected container will also be included. Defaults to FALSE.}
}
\value{
The response will contain a list of protocol objects, each of which will have the
following properties:
     \itemize{
  	      \item{ "name": Name of the saved protocol. }
  	      \item{ "description": Description of the saved protocol, if provided. }
  	      \item{ "xmlParameters": Bioml representation of the parameters defined by this protocol. }
  	      \item{ "jsonParameters": A list representation of the parameters defined by this protocol. }
  	      \item{ "containerPath": The container path where this protocol was saved. }
	}
The response will also include a defaultProtocolName property representing which of the protocol
names is the default.
}

\author{Cory Nathe}
\seealso{
\code{\link{labkey.pipeline.getPipelineContainer}},
\code{\link{labkey.pipeline.getFileStatus}},
\code{\link{labkey.pipeline.startAnalysis}}
}
\examples{
\dontrun{

labkey.pipeline.getProtocols(
    baseUrl="http://labkey/",
    folderPath="home",
    taskId = "pipelinetest:pipeline:r-copy",
    path = "r-copy",
    includeWorkbooks = FALSE
)

}
}
\keyword{IO}
