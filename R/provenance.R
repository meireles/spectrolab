################################################################################
# Minimal provenance: quantity and wavelength_unit
################################################################################

#' Get the physical quantity a spectra object measures
#'
#' \code{quantity} returns \code{"reflectance"}, \code{"radiance"}, or another
#' descriptive label captured or set for \code{x}. It is \code{NA} for objects
#' where this was never recorded (e.g. built by hand with \code{as_spectra()})
#' or where a transform (e.g. \code{\link{deriv_spectra}},
#' \code{\link{continuum_removal}}) has made the original label inapplicable.
#' \code{\link{read_spectra}} sets this from its \code{type} argument.
#'
#' @param x a spectra object
#' @return a single character string, or \code{NA_character_} if unset
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' dir_path = system.file("extdata", "Acer_example", package = "spectrolab")
#' spec = read_spectra(dir_path, format = "sig")
#' quantity(spec)
quantity = function(x){
    UseMethod("quantity")
}


#' @describeIn quantity Get the physical quantity a spectra object measures
#' @export
quantity.spectra = function(x){
    q = attr(x, "quantity")
    if(is.null(q)) NA_character_ else q
}


#' Set the physical quantity a spectra object measures
#'
#' @param x a spectra object
#' @param value a single character string (e.g. \code{"reflectance"},
#'        \code{"radiance"}), or \code{NA}/\code{NULL} to mark it unknown
#' @return \code{x}, with the quantity attribute set
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' quantity(spec) = "reflectance"
`quantity<-` = function(x, value){
    UseMethod("quantity<-")
}


#' @describeIn quantity<- Set the physical quantity a spectra object measures
#' @export
`quantity<-.spectra` = function(x, value){
    if(!is.null(value) && (!is.character(value) || length(value) != 1)){
        stop("quantity must be a single character string, NA, or NULL")
    }
    attr(x, "quantity") = value
    x
}


#' Get the wavelength unit of a spectra object's bands
#'
#' \code{wavelength_unit} returns the unit bands are expressed in --
#' \code{"nm"} by default (every currently supported instrument reports
#' nanometers), or whatever was captured/set for \code{x}.
#'
#' @param x a spectra object
#' @return a single character string
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' wavelength_unit(spec)
wavelength_unit = function(x){
    UseMethod("wavelength_unit")
}


#' @describeIn wavelength_unit Get the wavelength unit of a spectra object's bands
#' @export
wavelength_unit.spectra = function(x){
    u = attr(x, "wavelength_unit")
    if(is.null(u)) "nm" else u
}


#' Set the wavelength unit of a spectra object's bands
#'
#' @param x a spectra object
#' @param value a single character string (e.g. \code{"nm"}, \code{"um"}), or
#'        \code{NA}/\code{NULL} to mark it unknown
#' @return \code{x}, with the wavelength_unit attribute set
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' wavelength_unit(spec) = "nm"
`wavelength_unit<-` = function(x, value){
    UseMethod("wavelength_unit<-")
}


#' @describeIn wavelength_unit<- Set the wavelength unit of a spectra object's bands
#' @export
`wavelength_unit<-.spectra` = function(x, value){
    if(!is.null(value) && (!is.character(value) || length(value) != 1)){
        stop("wavelength_unit must be a single character string, NA, or NULL")
    }
    attr(x, "wavelength_unit") = value
    x
}


#' Reconcile a scalar provenance value between two spectra objects
#'
#' \code{i_reconcile_provenance_scalar} is shared by \code{\link{combine}} and
#' \code{\link{Ops.spectra}}: when a scalar provenance value (\code{quantity}
#' or \code{wavelength_unit}) agrees between two objects being merged, it is
#' kept; when it disagrees, a warning is raised and the result is cleared to
#' \code{NA} rather than silently keeping one side's value.
#'
#' @param v1,v2 the two values to reconcile (length-1 character or NA)
#' @param label name used in the warning message (e.g. \code{"quantity"})
#' @return the reconciled value
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_reconcile_provenance_scalar = function(v1, v2, label){
    if(identical(v1, v2)){
        return(v1)
    }
    if(is.na(v1) && is.na(v2)){
        return(NA_character_)
    }
    warning("`", label, "` differs between the two spectra (", v1, " vs ", v2,
             "); clearing it on the result.", call. = FALSE)
    NA_character_
}
