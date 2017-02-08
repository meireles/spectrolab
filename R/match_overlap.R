
#' Are wavelengths increasing
#'
#' Many transform functions can only (or at least should only) be applied to
#' spectra with monotonically varying, very likely increasing) wavelength values.
#' \code{i_test_increasing_wavelengths} tests that case and may throw an error
#' or return the boolen result from the test.
#'
#' @param x wavelengths
#' @param stop boolean. Throw error if test fails? Defaults to TRUE
#' @param call boolean. If stop = TRUE, should the function call be printed?
#' @return boolean
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_test_increasing_wavelengths = function(x, stop = TRUE, call = FALSE){
    y = all(diff(x) >= 0.0)
    if( !y && stop){
        stop("Wavelength values must be strictly increasing. You probably need to run `match_overlap` first", call. = call)
    }
    y
}


#' Find sensor overlap bounds
#'
#' \code{i_find_sensor_overlap_bounds} finds the overlap bounds between sensors
#'
#' @param x wavelength vector
#' @param idx boolean. return indices? defaults to TRUE
#' @return data.frame with sensor bounds
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_find_sensor_overlap_bounds = function(x, idx = TRUE){
    decrease    = which(diff(x) < 0.0)
    n_decreases = length(decrease) + 1
    dimnames    = list(c("begin", "end"),
                       paste("sensor", seq(n_decreases), sep = "_"))

    bounds = matrix(data     = c( c(1, decrease + 1), c(decrease, length(x)) ),
                    ncol     = n_decreases,
                    byrow    = TRUE,
                    dimnames = dimnames)
    if(!idx){
        bounds["begin", ] = x[ bounds["begin", ] ]
        bounds["end", ]   = x[ bounds["end", ] ]
    }
    as.data.frame(bounds)
}


i_remove_duplicated_wavelength = function(x, boundary){
    w = wavelengths(x)
    d = w[ which(duplicated(w)) ]

    if(length(d) > 1){
        stop("can only deal with a single duplicate.")
    }

    i      = which(w == d)
    idx_rm = ifelse(d < boundary, i[2], i[1])

    ## HACK. There is no easy way of subsetting wavelengths if they are
    ## duplicated. Therefore, I have to change the value of the wavelength
    ## I want to exclude and then remove that.

    ## Assign a dummy wavelength value to the wl to rm
    bogus                    = 12345678911121110987654321.0
    wavelengths(x)[ idx_rm ] = bogus

    ## Now prune the spectral data
    x[ , wavelengths(x)[ wavelengths(x) != bogus ] ]
}


#' Trim sensor overlap
#'
#' @param x spectra object
#' @param splice_at
#'
#' @return
#' @export
#'
#' @examples
i_trim_sensor_overlap = function(x, splice_at){

    x = i_remove_duplicated_wavelength(x = x, boundary =  splice_at[1])
    w = wavelengths(x)
    b = i_find_sensor_overlap_bounds(w)

    if(ncol(b) == 1){
        message("No overlap regions were found. Returning spectra unmodified...")
        return(x)
    }
    if(length(splice_at) != ncol(b) - 1){
        stop("number of cut_points must be equal to the number of overlaps.")
    }

    s = lapply(b, function(y){
        w[ seq.int(y[[1]], y[[2]]) ]
    })

    ## trim wavelength lists
    ## Takes care of **most** of the overlap, but see below
    for(i in 1:length(splice_at) ){
        right      = which(s[[i + 1]] >=  splice_at[i])
        s[[i + 1]] = s[[i + 1]][ right ]
        s[[i]]     = s[[i]][ s[[i]] < min(s[[i + 1]]) ]
    }

    list("spectra" = x[ , unlist(s) ],
         "sensor"  = rep(names(s), sapply(s, length)))
}


#' Match spectra at sensor transitions
#'
#' @param x spectra object
#' @param splice_at wavelengths that serve as splice poits. Typically the
#'                  beginings of sensor 2 and sensor 3.
#' @param interpolate_wvl
#' @param instrument
#' @return spectra object
#'
#' @author Jose Eduardo Meireles
#' @export
match_transition = function(x, splice_at, interpolate_wvl = 5){
    UseMethod("match_overlap")
}


#' @describeIn match_sensor_transition Match sensor ovelap regions
#' @export
match_transition.spectra = function(x, splice_at, interpolate_wvl = 5){

    w = wavelengths(x)

    if( ! i_test_increasing_wavelengths(x = w, stop = FALSE) ){
        y = i_trim_sensor_overlap(x = x, splice_at = splice_at)
        x = y$spectra              # reassign x
        w = wavelengths(x)         # reassign w
        s = split(w, y$sensor)
    } else {
        s = cut(x              = w,
                breaks         = c(min(w), splice_at, max(w)),
                include.lowest = TRUE,
                right          = FALSE,
                labels         = paste("sensor", seq(length(splice_at) + 1), sep = "_") )
        s = split(w, s)
    }


    ## Pick wavelengths by sensor to computer factors
    p1 = s$sensor_1[ s$sensor_1 >= splice_at[1] - interpolate_wvl]
    p21 = s$sensor_2[ s$sensor_2 <= splice_at[1] + interpolate_wvl ]
    p23 = s$sensor_2[ s$sensor_2 >= splice_at[2] - interpolate_wvl ]
    p3 = s$sensor_3[ s$sensor_3 < splice_at[2] + interpolate_wvl ]

    ## solve issues if any of the picks are empty
    if(length(p1) == 0){ p1 = max(s$sensor_1) }
    if(length(p21) == 0){ p21 = min(s$sensor_2) }
    if(length(p23) == 0){ p23 = max(s$sensor_2) }
    if(length(p3) == 0){ p3 = min(s$sensor_3) }

    ## compute factors
    f1 = rowMeans(reflectance(x[ , p21, simplify = FALSE])) /
         rowMeans(reflectance(x[ , p1, simplify = FALSE]))
    f3 = rowMeans(reflectance(x[ , p23, simplify = FALSE])) /
         rowMeans(reflectance(x[ , p3, simplify = FALSE]))


    ## Compute the factor matrices
    ## These functions need to be empirically derived. Current implementation
    ## is just a hack and should not be used in production code

    fm1 = sapply(f1, function(y){
        seq(1.0, y, length.out = length(s$sensor_1))
    })

    fm3 = sapply(f3, function(y){
        seq(y, 1.0, length.out = length(s$sensor_3))
    })

    ## Transform data
    x[ , s$sensor_1] = reflectance(x[ , s$sensor_1 ] ) * t(fm1)
    x[ , s$sensor_3] = reflectance(x[ , s$sensor_3 ] ) * t(fm3)

    x
}


#' #' TODO
#' #'
#' #' @param x TODO
#' #' @param cut_points TODO
#' #' @return TODO
#' #'
#' #' @keywords internal
#' #' @author Jose Eduardo Meireles
#' i_match_overlap_svc = function(x, cut_points){
#'
#'     # ---------- Forwarded message ----------
#'     # From: "Lawrence Slomer" <lslomer@spectravista.com>
#'     # Date: Jun 12, 2015 9:07 AM
#'     # Subject: Re: Fwd: Re: SVC HR-1024i Matching Algorithm
#'     # ---------------------------------------
#'     #
#'     # Good day. Tom Corl asked me to respond to your matching question.
#'     #
#'     # The main matching algorithm uses the average values of the
#'     # Si and SWIR1 radiance values within the transition region
#'     # to compute a scalar "matching factor". This matching factor
#'     # is then used to scale the Si region up or down, so that the
#'     # Si region data (which is not temperature controlled) is matched
#'     # to the more stable SWIR1 data (which *is* temperature controlled).
#'     #
#'     # This compensates for sensitivity changes in the Si detector
#'     # with temperature. This algorithm is based on the physics of the Si
#'     # detector. The matching factor affects all of the Si
#'     # detector data, but is scaled to have less effect at lower wavelengths
#'     # since Si sensitivity is affected less by temperature at lower wavelengths.
#'     #
#'     # The NIR-SWIR matching, if checked, is a purely cosmetic
#'     # further cleanup of the *just* the transition region radiance.
#'     # It has no effect outside the transition region. It simply combines
#'     # the Si and SWIR1 data mathematically on a sliding scale so that
#'     # the resulting data does not have a step. As I mentioned this is
#'     # purely cosmetic and does not have any basis in the physics of
#'     # the detectors.
#'     #
#'     # Matching is generally not necessary if your application is only
#'     # interested in reflectance, as long as you have let the instrument
#'     # stabilize and take reference scans at reasonable intervals. A small
#'     # step between Si and SWIR1 due to temperature will divide out (generally)
#'     # and result in smooth reflectance in that region.
#'     #
#'     # Matching requires "reasonable" amounts of energy in the transition
#'     # region; otherwise noise may cause the algorithm to incorrectly
#'     # shift the Si radiance values.
#'     #
#'     # Please let me know if this answers your question.
#'     #
#'     # --Larry
#'     # ---------------------------------------
#'
#'     ## List wavelengths per sensor
#'     w = wavelengths(x)
#'     b = i_find_sensor_overlap_bounds(w)
#'     s = lapply(b, function(y){
#'         w[ seq.int(y[[1]], y[[2]]) ]
#'     })
#'
#'     if(ncol(b) == 1){
#'         message("No overlap regions were found. Returning spectra unmodified...")
#'         return(x)
#'     }
#'     if(length(cut_points) != ncol(b) - 1){
#'         stop("number of cut_points must be equal to the number of overlaps.")
#'     }
#'
#'     ## Compute factors for the silicon sensor (1st one)
#'     ## The factors are computed across a range of wavelengths
#'     ## I think that the defaults in SVC are 990 (used for cut point) and 1000
#'     rg = c(cut_points[1], 1000)
#'     m  = as.matrix(x)
#'
#'     o1 = m[ , as.character(s[[1]][ s[[1]] > rg[1] & s[[1]] < rg[2] ]) ]
#'     o2 = m[ , as.character(s[[2]][ s[[2]] > rg[1] & s[[2]] < rg[2] ]) ]
#'     f  = rowMeans(o2) / rowMeans(o1)
#'
#'     ## Prune the wavelengths based on cut_points
#'     for(i in 1 : length(cut_points)){
#'         right      = which(s[[i + 1]] >=  cut_points[i])
#'         s[[i + 1]] = s[[i + 1]][ right ]
#'         s[[i]]     = s[[i]][ s[[i]] < min(s[[i + 1]]) ]
#'     }
#'
#'     ## Compute the factor matrix for the visible range "AFTER prunning"
#'     ## TODO
#'     ## This function needs to be empirically derived. Current implementation
#'     ## is just a hack and should not be used in production code
#'     fm = sapply(f, function(y){
#'         seq(1.0, y, length.out = length(s[[1]]))
#'     })
#'
#'
#'     ## Before scaling the prunned spectra, I need to check for duplicated
#'     ## wavelengths and exclude them. Because the duplicates can be on either
#'     ## sensor1 or 2, depending on cut_points[1], it is works out to exclude the
#'     ## non monotonically increasing wl.
#'     ## HACK
#'     g = x[ , unlist(s) ]
#'     d = which(duplicated(wavelengths(g)))
#'
#'     if(wavelengths(g)[d] < cut_points[1]){
#'         idx_rm = d
#'     } else {
#'         idx_rm = which(diff(wavelengths(g)) <= 0.0)
#'     }
#'
#'     ## Assign a dummy wavelength value to the wl to rm
#'     bogus  = 12345678911121110987654321.0
#'     wavelengths(g)[ idx_rm ] = bogus
#'
#'     ## now prune the spectral data
#'     g = g[ , wavelengths(g)[ wavelengths(g) != bogus ]  ]
#'
#'     ## Scale silicon sensor by factors
#'     g[ , s[[1]]] = reflectance(g[ , s[[1]] ] ) * t(fm)
#'
#'     g
#' }
