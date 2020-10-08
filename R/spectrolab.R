descrip             = readLines("DESCRIPTION")
spectrolab_version  = tolower(descrip[ grep("Version:", descrip) ])
spectrolab_citation = format(citation("spectrolab"), style = "text")
spectrolab_citation = gsub("_", "", spectrolab_citation)
spectrolab_citation = paste0(spectrolab_citation, "DOI: https://doi.org/10.5281/zenodo.3934575")

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
