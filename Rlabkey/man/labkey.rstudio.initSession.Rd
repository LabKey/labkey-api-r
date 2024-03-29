\name{labkey.rstudio.initSession}
\alias{labkey.rstudio.initSession}
\title{Initialize a RStudio session for LabKey integration using a time one request id}
\description{
LabKey-RStudio integration helper. Not intended for use outside RStudio.
}
\usage{
labkey.rstudio.initSession(requestId, baseUrl)
}
\arguments{
\item{requestId}{A one time request id generated by LabKey server for initializing RStudio}

\item{baseUrl}{server location including context path, if any. e.g. https://www.labkey.org/}
}
\examples{
\dontrun{
## RStudio console only
library(Rlabkey)
labkey.rstudio.initSession(requestId="a60228c8-9448-1036-a7c5-ab541dc15ee9",
    baseUrl="http://labkey/")

}
}
