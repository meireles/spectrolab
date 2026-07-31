#' Spectra Transpose
#'
#' spectra are not transposable. Transpose the value instead
#'
#' @param x spectra
#' @return No return value. Operation not allowed
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' s = as_spectra(spec_matrix_example, name_idx = 1)
#'
#' t(value(s))
#' t(as.matrix(s))
t.spectra = function(x){
    stop("spectra are not transposable. You can `t(value(x))` though.")
}

#' Arithmetic and comparison operators for spectra
#'
#' Overloads arithmetic (\code{+ - * / ^}) and comparison
#' (\code{== != < <= >= >}) operators for spectra via \code{Ops.}. Arithmetic
#' returns a spectra; comparison returns a logical matrix carrying sample and
#' band dimnames. Unary \code{+}/\code{-} (e.g. \code{-spec}) are supported.
#'
#' \strong{Shapes.} For \code{x OP z} (or \code{z OP x}) where \code{x} is an
#' \eqn{n \times m} spectra (n samples, m bands), \code{z} must be one of:
#' \itemize{
#'   \item a scalar -- applied elementwise;
#'   \item a length-\eqn{m} vector (or \eqn{1 \times m} matrix) -- applied per
#'         band, i.e. the same value to every sample;
#'   \item a length-\eqn{n} vector (or \eqn{n \times 1} matrix) -- applied per
#'         sample, i.e. the same value across every band;
#'   \item an \eqn{n \times m} matrix, or another spectra with identical bands
#'         -- applied elementwise.
#' }
#' Any other shape is an error rather than being silently recycled. When
#' \eqn{n = m} a bare length-\eqn{n} vector is ambiguous and is read as
#' per-sample (R's native recycling); pass a \eqn{1 \times m} matrix to force
#' the per-band reading.
#'
#' When both sides are spectra, sample names and metadata are kept only when
#' they agree between \code{e1} and \code{e2}; when they disagree they are
#' cleared (with a warning) rather than one side winning silently.
#'
#' @param e1 lhs
#' @param e2 rhs
#' @return a spectra for arithmetic operators; a logical matrix for comparison
#'         operators
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec  = as_spectra(spec_matrix_example, name_idx = 1)
#' spec1 = spec * 2
#' spec2 = spec + spec
#' all(spec1 == spec2)
Ops.spectra = function(e1, e2) {
    mathop = c("+", "-", "*", "/", "^")
    boolop = c("==", "!=", "<", "<=", ">=", ">")

    if( ! .Generic %in% c(mathop, boolop) ){ stop("Not implemented") }

    ## Unary +/- (e.g. -spec): e2 is not supplied at all in that case.
    if( missing(e2) ){
        if( ! .Generic %in% c("+", "-") ){
            stop("Unary '", .Generic, "' is not implemented for spectra")
        }
        if( .Generic == "-" ){
            e1[] = -value(e1)
        }
        return(e1)
    }

    op = get(.Generic)

    ## Both arguments are spectra: require identical shape and bands, operate
    ## elementwise, and reconcile names/metadata.
    if( is_spectra(e1) && is_spectra(e2) ){
        if( any(dim(e1) != dim(e2)) ){
            stop("incompatible spectra dimensions", call. = FALSE)
        }
        ## Same tolerance as combine(): an exact `!=` rejected band vectors that
        ## differ only by floating-point noise (1e-12) while combine() happily
        ## accepted them, so the two functions disagreed about what "the same
        ## bands" means.
        if( ! isTRUE(all.equal(bands(e1), bands(e2))) ){
            stop("band labels must be identical", call. = FALSE)
        }

        res = op(value(e1), value(e2))

        if(.Generic %in% boolop){
            dimnames(res) = list(names(e1), bands(e1))
            return(res)
        }

        e1[] = res

        if(!identical(names(e1), names(e2))){
            warning("sample names not identical: removing sample names...")
            names(e1) = rep(NA, dim(e1)["n_samples"])
        }
        if(!identical(meta(e1), meta(e2))){
            warning("metadata not identical: removing metadata...")
            e1$meta = i_meta(NULL, nrow(e1))
        }
        return(e1)
    }

    ## Exactly one argument is a spectra. Broadcast the other (z) against the
    ## value matrix under strict shape rules (see i_ops_conform); no silent
    ## recycling.
    spectra_first = is_spectra(e1)
    s = if(spectra_first){ e1 } else { e2 }
    z = if(spectra_first){ e2 } else { e1 }

    d = dim(s)
    n = d[["n_samples"]]
    m = d[["n_bands"]]
    V = value(s)
    Z = i_ops_conform(z, n, m)

    res = if(spectra_first){ op(V, Z) } else { op(Z, V) }

    if(.Generic %in% boolop){
        dimnames(res) = list(names(s), bands(s))
        return(res)
    }

    s[] = res
    s
}


#' Broadcast a non-spectra operand against a spectra value matrix
#'
#' \code{i_ops_conform} enforces the shape rules of \code{\link{Ops.spectra}}:
#' given the other operand \code{z} and the spectra dimensions \code{n} (samples)
#' and \code{m} (bands), it returns either a scalar or an \eqn{n \times m} matrix
#' formed by broadcasting \code{z}, or errors if \code{z}'s shape is not one of
#' the permitted forms (scalar, length-m per-band, length-n per-sample, or a
#' matching \eqn{n \times m} / \eqn{1 \times m} / \eqn{n \times 1} matrix). A
#' bare length-n vector is read as per-sample, which also resolves the
#' \eqn{n = m} ambiguous case (R's native recycling).
#'
#' @param z the non-spectra operand
#' @param n number of samples
#' @param m number of bands
#' @return a scalar or an \eqn{n \times m} numeric matrix
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_ops_conform = function(z, n, m){

    shape_err = function(){
        stop("incompatible shape: `z` in a spectra math/comparison must be a ",
             "scalar, a length-", m, " (per-band) or length-", n,
             " (per-sample) vector, an ", n, "x", m, ", 1x", m, " or ", n,
             "x1 matrix, or a spectra with identical bands.", call. = FALSE)
    }

    d = dim(z)

    if( !is.null(d) ){
        if( prod(d) == 1 ){                       ## 1x1 matrix ~ scalar
            return(as.vector(z))
        }
        if( length(d) != 2 ){
            shape_err()
        }
        if( d[1] == n && d[2] == m ){             ## full matrix, elementwise
            return(z)
        }
        if( d[1] == 1 && d[2] == m ){             ## 1 x m -> per band
            return(matrix(as.vector(z), nrow = n, ncol = m, byrow = TRUE))
        }
        if( d[1] == n && d[2] == 1 ){             ## n x 1 -> per sample
            return(matrix(as.vector(z), nrow = n, ncol = m))
        }
        shape_err()
    }

    L = length(z)

    if( L == 1 ){                                 ## scalar
        return(z)
    }
    if( L == n ){                                 ## per sample (incl. n == m)
        return(matrix(z, nrow = n, ncol = m))
    }
    if( L == m ){                                 ## per band
        return(matrix(z, nrow = n, ncol = m, byrow = TRUE))
    }
    shape_err()
}


#' Math group generic for spectra
#'
#' Applies base math functions (\code{abs}, \code{sqrt}, \code{log},
#' \code{round}, etc. -- see \code{?Math}) to the value matrix of a spectra
#' object, keeping bands/names/metadata/provenance. \code{cumsum},
#' \code{cumprod}, \code{cummax}, and \code{cummin} are applied row-wise
#' (cumulative across bands, per sample) since their default methods would
#' otherwise flatten the value matrix into a single vector.
#'
#' @param x spectra
#' @param ... additional arguments passed to the underlying math function
#'            (e.g. \code{digits} for \code{round}/\code{signif})
#' @return spectra object
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' spec_abs  = abs(spec)
#' spec_sqrt = sqrt(spec)
Math.spectra = function(x, ...){
    cum_generics = c("cumsum", "cumprod", "cummax", "cummin")

    if(.Generic %in% cum_generics){
        x[] = t(apply(value(x), 1, .Generic))
    } else {
        x[] = do.call(.Generic, list(value(x), ...))
    }

    x
}


#' Matrix multiplication for spectra
#'
#' \code{spectra \%*\% y} or \code{y \%*\% spectra} multiplies the value matrix
#' (with band/sample dimnames from \code{\link{as.matrix.spectra}}) against
#' \code{y}, returning a plain matrix -- not a \code{spectra} object, since the
#' result's rows/columns generally no longer correspond to samples/bands (e.g.
#' after projecting onto PCA loadings). Requires R >= 4.3, where \code{\%*\%}
#' became properly S3-generic in both argument positions (previously, a
#' spectra-on-the-right multiplication like \code{mat \%*\% spec} could not
#' dispatch to this method).
#'
#' @param x lhs
#' @param y rhs
#' @return a plain matrix
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' spec %*% t(as.matrix(spec))
`%*%.spectra` = function(x, y){
    if(is_spectra(x)){ x = as.matrix(x) }
    if(is_spectra(y)){ y = as.matrix(y) }
    x %*% y
}
