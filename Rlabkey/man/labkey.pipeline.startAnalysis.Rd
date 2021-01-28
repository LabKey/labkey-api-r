\name{labkey.pipeline.startAnalysis}
\alias{labkey.pipeline.startAnalysis}
\title{Start an analysis of a set of files using a pipeline}
\description{
 Starts analysis of a set of files using a particular
 protocol definition with a particular pipeline.
}
\usage{
labkey.pipeline.startAnalysis(baseUrl=NULL, folderPath,
    taskId, protocolName, path, files, fileIds = list(),
    pipelineDescription = NULL, protocolDescription = NULL,
    jsonParameters = NULL, xmlParameters = NULL,
    allowNonExistentFiles = FALSE, saveProtocol = TRUE)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl} for the LabKey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{taskId}{a string identifier for the pipeline}
  \item{protocolName}{a string name of the analysis protocol}
  \item{path}{a string for the relative path from the folder's pipeline root}
  \item{files}{a list of names of the files within the subdirectory described
    by the path property}
  \item{fileIds}{(optional) list of data IDs of files to be used as inputs for this pipeline.
    These correspond to the rowIds from the table ext.data. They do not need to be
    located within the file path provided. The user does need read access to the
    container associated with each file.}
  \item{pipelineDescription}{(optional) a string description displayed in the pipeline}
  \item{protocolDescription}{(optional) a string description of the analysis protocol}
  \item{jsonParameters}{(optional) a list of key / value pairs, or a JSON string representation,
    for the protocol description. Not allowed if a protocol with the same name has
    already been saved. If no protocol with the same name exists, either this property
    or xmlParameters must be specified.}
  \item{xmlParameters}{(optional) a string XML representation of the protocol description. Not
    allowed if a protocol with the same name has already been saved. If no protocol
    with the same name exists, either this property or jsonParameters must be specified.}
  \item{allowNonExistentFiles}{(optional) a boolean indicating if the pipeline should
    allow non existent files. Defaults to false.}
  \item{saveProtocol}{(optional) a boolean indicating if no protocol with this name
    already exists, whether or not to save this protocol definition for future use.
    Defaults to true.}
}
\value{
On success, the response will contain the jobGUID string value for the newly created pipeline job.
}

\author{Cory Nathe}
\seealso{
\code{\link{labkey.pipeline.getPipelineContainer}},
\code{\link{labkey.pipeline.getProtocols}},
\code{\link{labkey.pipeline.getFileStatus}}
}
\examples{
\donttest{

labkey.pipeline.startAnalysis(
    baseUrl="http://labkey/",
    folderPath="home",
    taskId = "pipelinetest:pipeline:r-copy",
    protocolName = "Test protocol name",
    path="r-copy",
    files = list("sample.txt", "result.txt"),
    protocolDescription = "Test protocol description",
    pipelineDescription = "test pipeline description",
    jsonParameters = list(assay = "Test assay name", comment = "Test assay comment"),
    saveProtocol = TRUE
)

}
}
\keyword{IO}
