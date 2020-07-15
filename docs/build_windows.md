## Building the Rlabkey Package on Windows

**Note: ** this requires using the cygwin dlls from the Rtools installation, rather than any preexisting cygwin installation.

* Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
    * Tested with Rtools35
    * Default components are correct
    * Do **not** unselect the Cygwin DLLs component
        * TCL/TK is not required
    * Allow installation to edit the system path
        * Click next. There is then an editable preview of the new system path. Leave as is.
        * If reinstalling Rtools, make sure there is only one copy of the new entries in the path

* Install [MiKTeX](http://www.miktex.org/download) for Latex support
    * After installation, open the MiKTeX package manager and install these additional packages:
        * fancyvrb
        * inconsolata
        * mptopdf
        * url

* Install [qpdf](https://sourceforge.net/projects/qpdf/files/)

* Run `./gradlew build`
