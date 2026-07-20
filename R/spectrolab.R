#' Spectrolab
#'
#' Class and methods for hyperspectral data.
#'
#' @keywords internal
"_PACKAGE"
NULL

.onAttach = function(libname, pkgname) {
    ## Compute version and citation at attach time (not build time) so we read
    ## the *installed* package metadata rather than a file in the working dir.
    version  = as.character(utils::packageVersion(pkgname))

    citation = tryCatch({
        cit = format(utils::citation(pkgname), style = "text")
        cit = gsub("_", "", cit)
        paste0(cit, "\nDOI: https://doi.org/10.5281/zenodo.3934575")
    }, error = function(e) {
        "Meireles et al. spectrolab. DOI: https://doi.org/10.5281/zenodo.3934575"
    })

    packageStartupMessage("spectrolab ", version, "\n\n",
                          "Please cite:\n", citation)
}
