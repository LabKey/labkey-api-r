\name{labkey.transform.getRunPropertyValue}
\alias{labkey.transform.getRunPropertyValue}
\title{Assay transform script helper function to get a run property value from a data.frame}
\description{
A function that takes in data.frame of the run properties info for a given assay transform script execution and
returns the value for a given property name.}
\usage{
labkey.transform.getRunPropertyValue(run.props, prop.name)
}
\arguments{
  \item{run.props}{the data.frame of the run property key/value pairs}
  \item{prop.name}{the name of the property to get the value of within the run.props data.frame}
}

\details{
This helper function will most likely be used within an assay transform script after the
labkey.transform.readRunPropertiesFile function has been called to load the full set of run properties.\cr
}
\examples{
\donttest{

# library(Rlabkey)

run.props = labkey.transform.readRunPropertiesFile("${runInfo}");
run.data.file = labkey.transform.getRunPropertyValue(run.props, "runDataFile");

}
}
