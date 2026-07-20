#' Summary group generic for spectra
#'
#' Handles \code{min}, \code{max}, \code{range}, \code{sum}, \code{prod},
#' \code{any} and \code{all} for spectra by applying the operation to the value
#' matrix. Non-spectra arguments are passed through unchanged, so mixed calls
#' such as \code{min(spec, 0.2)} or \code{max(spec1, spec2)} work exactly as they
#' do for base numeric objects --- unlike the previous per-function methods,
#' which errored on any extra argument.
#'
#' @param ... spectra and/or other objects accepted by the underlying generic
#' @param na.rm boolean. remove NAs? Defaults to FALSE
#' @return whatever the corresponding base generic returns (a single value, or a
#'         length-2 vector for \code{range})
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' min(spec)
#' max(spec)
#' range(spec)
Summary.spectra = function(..., na.rm = FALSE){
  args = lapply(list(...), function(z){ if(is_spectra(z)){ value(z) } else { z } })
  do.call(.Generic, c(args, list(na.rm = na.rm)))
}


#' Number of samples in a spectra object
#'
#' \code{length} returns the number of samples (rows), matching \code{nrow()}
#' and the first element of \code{dim()}. Without this method, \code{length()}
#' would fall back to counting spectra's four internal slots (value, bands,
#' names, meta) and always return 4, regardless of how many samples are in
#' the object.
#'
#' @param x spectra object
#' @return integer, the number of samples
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' length(spec) == nrow(spec)
length.spectra = function(x){
  nrow(x)
}


#' Find missing values in a spectra object
#'
#' \code{is.na} checks the value matrix for \code{NA}s, element by element.
#' Without this method, \code{is.na()} would check spectra's four internal
#' slots instead (always returning 4 \code{FALSE}s), which looks like "no
#' missing data" regardless of what the value matrix actually contains.
#'
#' @param x spectra object
#' @return logical matrix, same shape as \code{value(x)}
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' any(is.na(spec))
is.na.spectra = function(x){
  is.na(value(x))
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
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
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
#' @return `x`, invisibly (called for its side effect of printing)
#'
#' @importFrom utils head
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
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
  cat("bands: ", r_wvl[1], " to ", r_wvl[2], " ", wavelength_unit(x),
      " (", ncol(x), " bands",
      ifelse(!o_wvl, ", **overlap not matched**", ""),
      ")", "\n", sep = "")

  ## Minimal provenance (see R/provenance.R): only shown when known.
  q = quantity(x)
  if( !is.na(q) ){
    cat("quantity:", q, "\n")
  }

  if(l_met == 0){
    cat("metadata: none", "\n")
  } else {
    cat("metadata (", l_met, "): ", sep = "")
    cat(paste(n_met, collapse = ", "), "\n", sep = "")
  }

  ## Report captured sensor / detector-splice provenance, if any (see
  ## R/sensor_info.R). Summarises the instrument(s) and, for SVC, whether the
  ## overlap was preserved/removed and whether a matching factor was applied.
  si = sensor_info(x)
  if( !is.null(si) && nrow(si) > 0 ){
    instr = unique(stats::na.omit(si[["instrument"]]))
    if(length(instr) > 0){
      extra = ""
      modes = unique(stats::na.omit(si[["overlap_mode"]]))
      if(length(modes) > 0){
        matched = any(isTRUE(si[["matched"]]) | si[["matched"]] %in% TRUE, na.rm = TRUE)
        extra = paste0(" (overlap: ", paste(modes, collapse = "/"),
                       ", matched: ", ifelse(matched, "yes", "no"), ")")
      }
      cat("instrument: ", paste(instr, collapse = ", "), extra, "\n", sep = "")
    }
  }

  rw = min(nrow(x), 5L)
  l  = ncol(x)
  m  = 7L
  cl = min(l, m)
  s  = as.matrix(x)[seq.int(rw), seq.int(cl), drop = FALSE]

  if(l > m){
    s = cbind(s, "..." = "")
  }
  cat("\n")
  print(s, quote = FALSE)

  invisible(x)
}


#' Summarize spectra
#'
#' @param object spectra object
#' @param ... additional params to summary. not used yet
#' @return the spectra object, invisibly (prints a summary as a side effect)
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' summary(spec)
summary.spectra = function(object, ...){
  print(object, ...)
  invisible(object)
}


#' Structure of the spectra object
#'
#' @param object spectra object
#' @param ... additional args. not implemented
#' @return prints to console
#' @importFrom utils str
#'
#' @importFrom utils str
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' str(spec)
str.spectra = function(object, ...){
  str(unclass(object))
}
