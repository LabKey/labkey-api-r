\name{labkey.experiment.lineage}
\alias{labkey.experiment.lineage}
\title{Get lineage parent/child relationships for experiment objects}
\description{
Get lineage parent/child relationships and information for exp objects by LSID(s)
}
\usage{
labkey.experiment.lineage(baseUrl=NULL, folderPath,
    lsids, options = NULL)
}
\arguments{
  \item{baseUrl}{A string specifying the \code{baseUrl} for the labkey server.}
  \item{folderPath}{A string specifying the \code{folderPath} to be renamed. Additionally, the container entity id is also valid.}
  \item{lsids}{One or more LSID seed values for the experiment objects to retrieve lineage information for.}
  \item{options}{(optional) A list containing optional parameters specific to the lineage action.}
}
\details{
This function retrieves lineage parent/child relationships and information for experiment objects by LSID(s).

Optional parameters (passed via options) include:
    \itemize{
        \item{ \code{parents (boolean)} : include parent objects in the lineage}
        \item{ \code{children (boolean)} : include child objects in the lineage}
        \item{ \code{depth (integer)} : the depth of the lineage to retrieve}
        \item{ \code{expType (string)} : the type of experiment objects to retrieve lineage for}
        \item{ \code{cpasType (string)} : the type of CPAS object to retrieve lineage for}
        \item{ \code{runProtocolLsid (string)} : the LSID of the run protocol to retrieve lineage for}
        \item{ \code{includeProperties (boolean)} : include properties in the lineage response}
        \item{ \code{includeInputsAndOutputs (boolean)} : include inputs and outputs in the lineage response}
        \item{ \code{includeRunSteps (boolean)} : include run steps in the lineage response}
    }
}
\value{
Returns a lineage response object based on the LSID seed values provided. The response object contains:
    \itemize{
        \item{ \code{seeds} : the LSID of the object(s) requested}
        \item{ \code{nodes} : an object with LSID keys for each lineage node and values containing the lineage information}
    }
}

\author{Cory Nathe}
\examples{
\dontrun{

library(Rlabkey)

labkey.experiment.lineage(
  baseUrl="http://labkey/",
  folderPath = "/home/OriginalFolder",
  lsids=c("urn:lsid:labkey.com:Sample.519.Blood:23", "urn:lsid:labkey.com:Sample.519.Blood:12"),
  options=list(
    parents=TRUE,
    children=TRUE,
    depth=10,
    expType="Material",
    includeProperties=TRUE,
    includeInputsAndOutputs=FALSE,
    includeRunSteps=FALSE
  )
)

}
}
\keyword{IO}
