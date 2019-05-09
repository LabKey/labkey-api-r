library(Rlabkey)

# This file is included in the PR to illustate labkey.webdav.  This should be removed before final merge, 
# and a version of this should probably go into SVN and incorporated into 
#labkey.setDebugMode(T)

#setup
baseUrl <- "http://localhost:8080/labkey/"
fileRoot <- "/labkey_trunk/build/deploy/files/"

baseUrl <- "https://prime-seq.ohsu.edu/"
fileRoot <- NA

folderPath <- "home"

localDownloadDir <- "LocalTestDir"
dirName <- "TestDir-a%b#c&d@e/~2"  #Add tricky characters
dirNameEncoded <- "TestDir-a%25b%23c%26d%40e/%7E2"

fileName1 <- paste0(dirName, "/foo.txt")

localName <- "localCopy.txt"
localName2 <- "overwrittenRemoteFile.txt"

assertRemoteFileExists <- function(remoteFilePath) {
  if (!labkey.webdav.pathExists(baseUrl=baseUrl, folderPath = folderPath, remoteFilePath=remoteFilePath)) {
    stop(paste0("Remote file not found: ", remoteFilePath))
  }
  
  # Verify file exists in server filesystem (if accessible):
  if (!is.na(fileRoot)){
    expectedFile <- paste0(fileRoot, folderPath, "@files/", remoteFilePath)
    if (!file.exists(expected)) {
      stop(paste0("Uploaded file not found under server file root: ", remoteFilePath))
    }
  }
}

assertRemoteFileDoesNotExist <- function(remoteFilePath) {
  if (labkey.webdav.pathExists(baseUrl=baseUrl, folderPath = folderPath, remoteFilePath=remoteFilePath)) {
    stop(paste0("Remote file found: ", remoteFilePath))
  }
  
  # Verify file exists in server filesystem (if accessible):
  if (!is.na(fileRoot)){
    expectedFile <- paste0(fileRoot, folderPath, "@files/", remoteFilePath)
    if (file.exists(expected)) {
      stop(paste0("File still present under server file root: ", remoteFilePath))
    }
  }
}

assertLocalFileExists <- function(filePath) {
  if (!file.exists(filePath)) {
    stop(paste0("Expected file not found: ", filePath))
  }
}

cleanup <- function(){
  print("Cleaning")
  if (file.exists(localDownloadDir)){
    print(paste0("Removing directory: ", localDownloadDir))
    unlink(localDownloadDir, recursive = T)
  }
  
  if (labkey.webdav.pathExists(baseUrl=baseUrl, folderPath = folderPath, remoteFilePath=dirName)) {
    print("Deleting remote directory")
    labkey.webdav.delete(baseUrl=baseUrl, folderPath = folderPath, remoteFilePath=dirName)
  }
  assertRemoteFileDoesNotExist(dirName)
  
  if (file.exists(localName)){
    unlink(localName)
  }
  
  if (file.exists(localName2)){
    unlink(localName2)
  }
}

# pre-clean
cleanup()

# Attempt to download non-existant file:
tryCatch({
  labkey.webdav.get(baseUrl=baseUrl, folderPath=folderPath, remoteFilePath='fakeFile.txt', localFilePath = 'fakeFile.txt')
  stop("This should not have worked")
}, error = function(e){
  print("This failed as expected")  
})


# Create local folder
dir.create(localDownloadDir, recursive = T)

# Create remote folder
labkey.webdav.mkDirs(baseUrl=baseUrl, folderPath, remoteFilePath=dirName)
assertRemoteFileExists(remoteFilePath=dirName)
  
# Create file and upload
filePath <- tempfile(pattern = "lkWebDav", tmpdir = tempdir(), fileext = "")
fileConn<-file(filePath)
writeLines(c("Hello","World"), fileConn)
close(fileConn)

#failed put:
tryCatch({
  labkey.webdav.put(baseUrl=baseUrl, paste0(filePath, "failure"), folderPath=folderPath, remoteFilePath=fileName1)
  stop("This should not have worked")
}, error = function(e){
  print("This failed as expected")  
})

labkey.webdav.put(baseUrl=baseUrl, filePath, folderPath=folderPath, remoteFilePath=fileName1)
assertRemoteFileExists(remoteFilePath=fileName1)

# Download this file
labkey.webdav.get(baseUrl=baseUrl, folderPath=folderPath, remoteFilePath=fileName1, localFilePath=localName, overwrite=TRUE)
assertLocalFileExists(localName)

# Now try to re-download using overwrite=F
fileChars1 <- readChar(localName,nchars=1e6)

labkey.webdav.get(baseUrl=baseUrl, folderPath=folderPath, remoteFilePath=fileName1, localFilePath=localName, overwrite=FALSE)
fileChars2 <- readChar(localName,nchars=1e6)

# Should be equal
if (fileChars1 != fileChars2) {
  stop("Files were not equal, file was overwritten")
}

# Alter the local file:
fileConn<-file(localName)
writeLines(c("I was changed"), fileConn)
close(fileConn)
fileChars1 <- readChar(localName,nchars=1e6)

labkey.webdav.get(baseUrl=baseUrl, folderPath=folderPath, remoteFilePath=fileName1, localFilePath=localName, overwrite=TRUE)
fileChars2 <- readChar(localName,nchars=1e6)

# Should be changed
if (fileChars1 == fileChars2) {
  stop("Files were equal, file should have been was overwritten")
}

# Alter the local file again:
fileConn<-file(localName)
writeLines(c("I was changed"), fileConn)
close(fileConn)
fileChars1 <- readChar(localName,nchars=1e6)

#now upload again:
labkey.webdav.put(baseUrl=baseUrl, localName, folderPath=folderPath, remoteFilePath=fileName1)
assertRemoteFileExists(remoteFilePath=fileName1)

labkey.webdav.get(baseUrl=baseUrl, folderPath=folderPath, remoteFilePath=fileName1, localFilePath=localName2, overwrite=TRUE)
fileChars1 <- readChar(localName,nchars=1e6)
fileChars2 <- readChar(localName2,nchars=1e6)

if (fileChars1 != fileChars2) {
  stop("Remote file should have been overwritten")
}

# Make multiple folders:
remoteDir2 <- paste0(dirName, "/1/2/3")
labkey.webdav.mkDirs(baseUrl = baseUrl, folderPath = folderPath, remoteFilePath=remoteDir2)
assertRemoteFileExists(remoteFilePath=remoteDir2)

# Test listDir
ret <- labkey.webdav.listDir(baseUrl=baseUrl, folderPath=folderPath, remoteFilePath=dirName)
if (length(ret[["files"]]) != 2) {
  stop (paste0("Expected 2 files, was: ", length(ret[["files"]])))
}

if (ret[["fileCount"]] != 2) {
  stop ("Expected 2 files")
}

expectedJson <- list(
  list("id"=paste0("/_webdav/home/@files/", dirName, "/1"),
       "href"=paste0("/_webdav/home/%40files/", dirNameEncoded, "/1/"),
       "text"="1",
       "isdirectory"=TRUE
       ),
  list("id"=paste0("/_webdav/home/@files/", dirName, "/foo.txt"),
       "href"=paste0("/_webdav/home/%40files/", dirNameEncoded, "/foo.txt"),
       "text"="foo.txt",
       "isdirectory"=FALSE
  )
)

for (idx in c(1,2)) {
  expected <- expectedJson[[idx]]
  actual <- ret[["files"]][[idx]]
  for (prop in names(expected)) {
    if (expected[[prop]] != actual[[prop]]) {
      stop(paste0("Incorrect value for: ", prop, " was: ", actual[[prop]], ". expected: ", expected[[prop]]))
    }
  }
  
  for (prop in c("creationdate", "createdby")) {
    if (is.null(actual[[prop]])) {
      stop(paste0("Missing value for: ", prop))
    }
  }
  
  #file/directory specific props
  for (prop in c("contentlength", "size", "lastmodified")) {
    if (actual$isdirectory) {
      if (!is.null(actual[[prop]])) {
        stop(paste0("Not null value for: ", prop))
      }
    } else {
      if (is.null(actual[[prop]])) {
        stop(paste0("Missing value for: ", prop))
      }
    }
  }
}

# Try delete
remoteDir3 <- paste0(dirName, "/1/4")
labkey.webdav.mkDirs(baseUrl = baseUrl, folderPath = folderPath, remoteFilePath=remoteDir3)
assertRemoteFileExists(remoteFilePath=remoteDir3)

labkey.webdav.delete(baseUrl = baseUrl, folderPath = folderPath, remoteFilePath=remoteDir3)
assertRemoteFileDoesNotExist(remoteFilePath=remoteDir3)

# Download directory
labkey.webdav.downloadFolder(localDownloadDir, baseUrl, folderPath = folderPath, remoteFilePath = dirName)
assertLocalFileExists(file.path(localDownloadDir, dirName))
assertLocalFileExists(file.path(localDownloadDir, fileName1))
assertLocalFileExists(file.path(localDownloadDir, dirName, "1"))
assertLocalFileExists(file.path(localDownloadDir, dirName, "1/2/3"))

cleanup()

