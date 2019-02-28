library(Rlabkey)

filePath <- tempfile(pattern = "lkWebDav", tmpdir = tempdir(), fileext = "")
 
fileConn<-file(filePath)
writeLines(c("Hello","World"), fileConn)
close(fileConn)

baseUrl <- 'http://localhost:8080/labkey/'
folderPath <- '/home'

fileRoot <- '/labkey_trunk/build/deploy/files/'
expectedFile <- paste0(fileRoot, 'home/@files/myFile.txt')

#labkey.setDebugMode(T)

remoteName <- 'foo.txt'
labkey.webdav.put(baseUrl=baseUrl, filePath, folderPath=folderPath, fileSet="@files", remoteFilePath=remoteName, overwriteRemote=TRUE)
#file.exists(expected)

localName <- 'localCopy.txt'
if (file.exists(localName)){
  file.remove(localName)
}

labkey.webdav.get(baseUrl=baseUrl, folderPath=folderPath, fileSet="@files", remoteFilePath=remoteName, localFilePath=localName, overwrite=TRUE)
file.exists(fileName)

labkey.webdav.get(baseUrl=baseUrl, folderPath=folderPath, fileSet="@files", remoteFilePath=remoteName, localFilePath=localName, overwrite=FALSE)
file.exists(fileName)
