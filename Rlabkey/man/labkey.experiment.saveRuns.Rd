\name{labkey.experiment.saveRuns}
\alias{labkey.experiment.saveRuns}
\title{Saves Runs.}
\description{
Saves experiment runs.
}
\usage{
labkey.experiment.saveRuns(baseUrl=NULL, folderPath,
    protocolName, runList)
}
\arguments{
  \item{baseUrl}{(optional) a string specifying the \code{baseUrl} for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{protocolName}{ a string specifying the protocol name of the protocol to use}
  \item{runList}{a list of experiment run objects}
}
\details{
Saves experiment runs. Runs may refer to existing data and material objects, either inputs or outputs,
by ID or LSID. Runs may also define new data and materials objects by not specifying an ID or LSID in their properties.

Runs can be created for either assay or non-assay backed protocols. For an assay backed protocol, either the assayId or
the assayName and providerName name must be specified in the assayConfig parameter. If a non-assay backed protocol is to be used,
specify the protocolName string value, note that currently only the simple : labkey.experiment.SAMPLE_DERIVATION_PROTOCOL is supported.

Refer to the labkey.experiment.createData, labkey.experiment.createMaterial, and labkey.experiment.createRun helper functions
to assemble the data structure that saveRuns expects.
}

\value{
 Returns the object representation of the experiment run.
}

\author{Ankur Juneja}
\seealso{
\code{\link{labkey.experiment.createData}},
\code{\link{labkey.experiment.createMaterial}},
\code{\link{labkey.experiment.createRun}}
}
\examples{
\donttest{

library(Rlabkey)

## uploads data to an existing assay

df <- data.frame(participantId=c(1:3), visitId = c(10,20,30), sex = c("f", "m", "f"))
run <- labkey.experiment.createRun(list(name="new assay run"), dataRows = df)
labkey.experiment.saveRuns(baseUrl="http://labkey/", folderPath="home",
    assayConfig=list(assayName="GPAT", providerName="General"), runList=run)

## create a non-assay backed run with samples as material inputs and outputs

m1 <- labkey.experiment.createMaterial(
        list(name = "87444063.2604.626"), sampleSetName = "Study Specimens")
m2 <- labkey.experiment.createMaterial(
        list(name = "87444063.2604.625"), sampleSetName = "Study Specimens")
run <- labkey.experiment.createRun(
        list(name="new run"), materialInputs = m1, materialOutputs = m2)
labkey.experiment.saveRuns(baseUrl="http://labkey/", folderPath="home",
        protocolName=labkey.experiment.SAMPLE_DERIVATION_PROTOCOL, runList=run)
}
}
\keyword{IO}
