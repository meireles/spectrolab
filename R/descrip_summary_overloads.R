#' Minimum value
#'
#' \code{min} Returns the minimum value value in a spectra object
#'
#' @param ... spectra object
#' @param na.rm boolean. remove NAs? Defaults to FALSE
#' @return single numeric value
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' min(spec)
min.spectra = function(..., na.rm = FALSE){
  min(value(...), na.rm = na.rm)
}

#' Maximum value
#'
#' \code{max} Returns the maximum value value in a spectra object
#'
#' @param ... spectra object
#' @param na.rm boolean. remove NAs? Defaults to FALSE
#' @return single numeric value
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' max(spec)
max.spectra = function(..., na.rm = FALSE){
  max(value(...), na.rm = na.rm)
}

#' Range of value values
#'
#' \code{range} Returns the range of (min, max) value values in spectra
#'
#' @param ... spectra object
#' @param na.rm boolean. remove NAs? Defaults to FALSE
#' @return tuple of numeric values (min, max)
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#'
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' range(spec)
range.spectra = function(..., na.rm = FALSE){
  range(value(...), na.rm = na.rm)
}


#' Get dimension of spectra
#'
#' \code{dim} returns a vector with number of samples and bands (bands)
#'
#' @param x spectra object
#' @return tuple of integers: c("n_samples", "n_bands")
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' dim(spec)
dim.spectra = function(x){
  c("n_samples"     = length(names(x)),
    "n_bands" = length(bands(x)) )
}

#' Print spectra
#'
#' \code{print} prints basic information about the spectra obj to the console
#'
#' @param x spectra object
#' @param ... other arguments passed to print. not implemented for spectra
#' @return nothing. called for side effect
#'
#' @importFrom utils head
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' print(spec)
#' ## or simply
#' spec
print.spectra = function(x, ...){
  w       = bands(x)
  r_wvl   = range(w)
  o_wvl   = i_is_increasing(w)

  n_met   = names(meta(x, simplify = FALSE))
  l_met   = length(n_met)
  l_max   = 3L

  if(l_met > l_max){
    n_met = c( utils::head(n_met, l_max) , "...")    ## overwriting n_met
    l_met = paste(l_max, "of", l_met)                ## overwriting l_met
  }

  cat("spectra object", "\n")
  cat("number of samples:", nrow(x),"\n")
  cat("bands: ", r_wvl[1], " to ", r_wvl[2], " (", ncol(x), " bands",
      ifelse(!o_wvl, ", **overlap not matched**", ""),
      ")", "\n", sep = "")

  if(l_met == 0){
    cat("metadata: none", "\n")
  } else {
    cat("metadata (", l_met, "): ", sep = "")
    cat(paste(n_met, collapse = ", "), sep = "")
  }
}


#' Summarize spectra
#'
#' @param object spectra object
#' @param ... additional params to summary. not used yet
#' @return nothing yet (just prints to console)
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' summary(spec)
summary.spectra = function(object, ...){
  print(object, ...)
}


#' Structure of the spectra object
#'
#' @param object spectra object
#' @param ... additional args. not implemented
#' @return prints to console
#' @importFrom utils str
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' str(spec)
str.spectra = function(object, ...){
  str(unclass(object))
}
