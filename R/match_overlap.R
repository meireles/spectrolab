#' Find sensor bounds
#'
#' \code{i_find_sensor_bounds} finds the bounds bwteen sen
#'
#' @param x wavelength vector
#' @param idx boolean. return indices? defaults to TRUE
#' @return data.frame with sensor bounds
#'
#' @author Jose Eduardo Meireles
i_find_sensor_bounds= function(x, idx = TRUE){
    y = which(diff(x) < 0.0)
    n = length(y) + 1
    l = matrix(data     = c( c(1, y + 1), c(y, length(x)) ),
               ncol     = n,
               byrow    = TRUE,
               dimnames = list(c("begin", "end"),
                               paste("sensor", seq(n), sep = "_") )
    )

    if(!idx){
        l["begin", ] = x[ l["begin", ] ]
        l["end", ]   = x[ l["end", ] ]
    }
    as.data.frame(l)
}


#' Match spectra at sensor overlap regions
#'
#' @param x spectra object
#' @param cut_points cut points for transition between sensors. must be of length
#'                   number of sensors - 1. Defaults to c(990, 1900)
#' @return spectra object
#'
#' @author Jose Eduardo Meireles
#' @export
match_overlap = function(x, cut_points = c(990, 1900) ){
    UseMethod("match_overlap")
}


#' @describeIn match_overlap Match sensor ovelap regions
#' @export
match_overlap.spectra = function(x, cut_points = c(990, 1900)){
    i_match_overlap_svc(x = x, cut_points = cut_points)
}


#' TODO
#'
#' @param x TODO
#' @param cut_points TODO
#' @return TODO
#'
#' @author Jose Eduardo Meireles
i_match_overlap_svc = function(x, cut_points){

    # ---------- Forwarded message ----------
    # From: "Lawrence Slomer" <lslomer@spectravista.com>
    # Date: Jun 12, 2015 9:07 AM
    # Subject: Re: Fwd: Re: SVC HR-1024i Matching Algorithm
    # ---------------------------------------
    #
    # Good day. Tom Corl asked me to respond to your matching question.
    #
    # The main matching algorithm uses the average values of the
    # Si and SWIR1 radiance values within the transition region
    # to compute a scalar "matching factor". This matching factor
    # is then used to scale the Si region up or down, so that the
    # Si region data (which is not temperature controlled) is matched
    # to the more stable SWIR1 data (which *is* temperature controlled).
    #
    # This compensates for sensitivity changes in the Si detector
    # with temperature. This algorithm is based on the physics of the Si
    # detector. The matching factor affects all of the Si
    # detector data, but is scaled to have less effect at lower wavelengths
    # since Si sensitivity is affected less by temperature at lower wavelengths.
    #
    # The NIR-SWIR matching, if checked, is a purely cosmetic
    # further cleanup of the *just* the transition region radiance.
    # It has no effect outside the transition region. It simply combines
    # the Si and SWIR1 data mathematically on a sliding scale so that
    # the resulting data does not have a step. As I mentioned this is
    # purely cosmetic and does not have any basis in the physics of
    # the detectors.
    #
    # Matching is generally not necessary if your application is only
    # interested in reflectance, as long as you have let the instrument
    # stabilize and take reference scans at reasonable intervals. A small
    # step between Si and SWIR1 due to temperature will divide out (generally)
    # and result in smooth reflectance in that region.
    #
    # Matching requires "reasonable" amounts of energy in the transition
    # region; otherwise noise may cause the algorithm to incorrectly
    # shift the Si radiance values.
    #
    # Please let me know if this answers your question.
    #
    # --Larry

    ## List wavelengths per sensor
    w = wavelengths(x)
    b = i_find_sensor_bounds(w)
    s = lapply(b, function(y){
        w[ seq.int(y[[1]], y[[2]]) ]
    })

    if(length(cut_points) != ncol(b) - 1){
        stop("number of cut_points must be equal to the number of overlaps")
    }

    ## Compute factors for the silicon sensor (1st one)
    ## The factors are computed across a range of wavelengths
    ## I think that the defaults in SVC are 990 (used for cut point) and 1010
    rg = c(cut_points[1], 1010)
    m  = as.matrix(x)

    o1 = m[ , as.character(s[[1]][ s[[1]] > rg[1] & s[[1]] < rg[2] ]) ]
    o2 = m[ , as.character(s[[2]][ s[[2]] > rg[1] & s[[2]] < rg[2] ]) ]
    f  = rowMeans(o2) / rowMeans(o1)

    ## Prune the wavelengths based on cut_points
    for(i in 1 : length(cut_points)){
        right      = which(s[[i + 1]] >=  cut_points[i])
        s[[i + 1]] = s[[i + 1]][ right ]
        s[[i]]     = s[[i]][ s[[i]] < min(s[[i + 1]]) ]
    }

    ## Compute the factor matrix for the visible range "AFTER prunning"
    fm = sapply(f, function(y){
        seq(1.0, y, length.out = length(s[[1]]))
    })

    ## Before scaling the prunned spectra, I need to check for duplicated
    ## wavelengths and exclude them. Because the duplicates can be on either
    ## sensor1 or 2, depending on cut_points[1], it is works out to exclude the
    ## non monotonically increasing wl.
    ## HACK
    g      = x[ , unlist(s) ]
    idx_rm = which(diff(wavelengths(g)) <= 0.0)

    ## Assign a dummy wavelength value to the wl to rm
    bogus  = 123456789
    wavelengths(g)[ idx_rm ] = bogus

    ## now prune the spectral data
    g = g[ , wavelengths(g)[ wavelengths(g) != bogus ]  ]

    ## Scale silicon sensor by factors
    g[ , s[[1]]] = reflectance(g[ , s[[1]] ] ) * t(fm)

    g
}
