devtools::use_package("devtools")
library("devtools")

descrip            = readLines("DESCRIPTION")
spectrolab_version = tolower(descrip[ grep("Version:", descrip) ])
spectrolab_citation = format(citation("spectrolab"), style = "text")
spectrolab_citation = gsub("_", "", spectrolab_citation)

#' Spectrolab
#'
#' Class and methods for hyperspectral data.
#'
#' @docType package
#' @name spectrolab
NULL

.onAttach = function(libname, pkgname) {
    packageStartupMessage("spectrolab ",
                          spectrolab_version, "\n\n",
                          "Please cite:\n",
                          spectrolab_citation)
}
