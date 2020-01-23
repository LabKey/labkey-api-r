This file describes the sources for the Rlabkey package and a number of additional files and folders for developers intending to build the Rlabkey package.  


## Contents of labkey-api-r:
- **Rlabkey.Rcheck**	Temporary folder created by "check" target of build, contains intermediate files for validation checks
- **build**		Directory where packages are built, not checked into project.
- **latest**		Contains latest built packages checked in to project
- **test**			Contains R script file designed to connect to a LabKey instance on localhost with a project named apisamples. The list dataset referenced by this test can be created by importing the list archive listArchive.zip
 - **Note:** The "latest" and "test" folders are also in the former svn location (remoteapi/r). This is necessary at this time for running automated tests and is subject to change.
- **docs**			The source files for the Users Guide, plus a latex doc and corresponding Pdf describing the package build environment.
			
## Rlabkey: Root of all source code for both documentation and R code.
- **DESCRIPTION**	Text manifest file specifying version number, dependencies, and other properties.  Developer maintained.
- **NAMESPACE**	Specifies the public function names.  Developer maintained.
- **NEWS**	Document of change history for this project, developer maintaind.
- **inst/doc**	Folder for documents
 - **usersguide.pdf**			The pdf version of the source document found in \lktrunk\remoteapi\r\docs, printed to pdf format
- **man**		Source files for function-by-function reference documentation.  After a successful build, a final pdf of the function reference documentation can be found in Rlabkey.Rcheck/Rlabkey-manual.pdf.
- **R**		R Source files for package functions
- **src**		Source files for a faster implementation of Json parsing for retrieved data.  Written in C, build occurs at package build time for windows,	and at package install time for unix
			
--------------------------------------------------------------------------------------------------------------------

## Building:Rlabkey

Install all the components needed, as described in docs/updateRPkgs.pdf.
**Note:** This document is outdated and none of the links are correct. All operating systems need Rtools and latex installed. Rtools is available via
direct download or instructions [here](https://cran.r-project.org/). OS specific instructions:
- [Windows](docs/build_windows.md)
- Others TBD

After all tools are installed, run ant check, then ant build.


