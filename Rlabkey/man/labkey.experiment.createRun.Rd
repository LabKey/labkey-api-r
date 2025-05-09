\name{labkey.experiment.createRun}
\alias{labkey.experiment.createRun}
\title{Create an experiment run object}
\description{
Create an experiment run object.
}
\usage{
labkey.experiment.createRun(config,
    dataInputs = NULL, dataOutputs = NULL, dataRows = NULL,
    materialInputs = NULL, materialOutputs = NULL)
}
\arguments{
  \item{config}{A list of base experiment object properties. Note that run domain properties must be added to a properties sub-object.}
  \item{dataInputs}{(optional) a list of experiment data objects to be used as data inputs to the run}
  \item{dataOutputs}{(optional) a list of experiment data objects to be used as data outputs to the run}
  \item{dataRows}{(optional) a data frame containing data rows to be uploaded to the assay backed run}
  \item{materialInputs}{(optional) a list of experiment material objects to be used as material inputs to the run}
  \item{materialOutputs}{(optional) a list of experiment material objects to be used as material outputs to the run}
}
\details{
Create an experiment run object which can be used in the saveBatch function.
}

\value{
 Returns the object representation of the experiment run object.
}

\author{Karl Lum}
\seealso{
\code{\link{labkey.experiment.saveBatch}},
\code{\link{labkey.experiment.createData}},
\code{\link{labkey.experiment.createMaterial}}
}
\examples{
\dontrun{

library(Rlabkey)

## create a non-assay backed run with samples as material inputs and outputs

m1 <- labkey.experiment.createMaterial(
        list(name = "87444063.2604.626"), sampleSetName = "Study Specimens")
m2 <- labkey.experiment.createMaterial(
        list(name = "87444063.2604.625"), sampleSetName = "Study Specimens")

## create the run configuration and include optional run level properties
runConfig <- list(name="new run", properties <- list(textField="run prop 1", floatField=3.14))

run <- labkey.experiment.createRun(
        runConfig, materialInputs = m1, materialOutputs = m2)
labkey.experiment.saveBatch(baseUrl="http://labkey/", folderPath="home",
        protocolName=labkey.experiment.SAMPLE_DERIVATION_PROTOCOL, runList=run)

}
}
\keyword{IO}
