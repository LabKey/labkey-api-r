\name{labkey.setWafEncoding}
\alias{labkey.setWafEncoding}
\title{Helper function to enable/disable wafEncoding mode.}
\description{
By default, this command encodes the SQL parameter to allow it to pass through web application
firewalls. This is compatible with LabKey Server v23.9.0 and above. If targeting an earlier
server, pass FALSE to this method.
}
\usage{
labkey.setWafEncoding(wafEncode = TRUE)
}
\arguments{
  \item{wafEncode}{a boolean specifying if wafEncode mode is enabled or disabled}
}

\author{Cory Nathe}
\examples{
\dontrun{

library(Rlabkey)
labkey.setWafEncoding(FALSE)
labkey.executeSql(
    baseUrl="http://localhost:8080/labkey",
    folderPath="/home",
    schemaName="core",
    sql = "select * from containers")

}
}
\keyword{IO}
