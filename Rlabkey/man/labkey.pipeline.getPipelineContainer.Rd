\name{labkey.pipeline.getPipelineContainer}
\alias{labkey.pipeline.getPipelineContainer}
\title{Gets the container in which the pipeline is defined}
\description{
 Gets the container in which the pipeline for this container is defined. This may be the
 container in which the request was made, or a parent container if the pipeline was defined
 there.
}
\usage{
labkey.pipeline.getPipelineContainer(baseUrl=NULL, folderPath)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl} for the LabKey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
}
\value{
The response will contain the following:
     \itemize{
  	      \item{ "containerPath": The container path in which the pipeline is defined.
                If no pipeline has been defined in this container hierarchy,
                then the value of this property will be null.
  	       }
  	      \item{ "webDavURL": The WebDavURL for the pipeline root. }
	}
}

\author{Cory Nathe}
\seealso{
\code{\link{labkey.pipeline.getProtocols}},
\code{\link{labkey.pipeline.getFileStatus}},
\code{\link{labkey.pipeline.startAnalysis}}
}
\examples{
\donttest{

labkey.pipeline.getPipelineContainer(
    baseUrl="http://labkey/",
    folderPath="home"
)

}
}
\keyword{IO}
