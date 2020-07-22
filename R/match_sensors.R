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

#' Trim sensor overlap
#'
#' @param x spectra object
#' @param splice_at bands where to splice sensors. suggests where the
#'                  beginning of sensors 2 and 3 should be.
#' @return spectra object
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_trim_sensor_overlap = function(x, splice_at){

    w = bands(x)
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
#' \code{match_sensors} scales values of sensors 1 (vis) and 3 (swir2)
#'
#' Splice_at has no default because sensor transition points vary between vendors
#' and individual instruments. It is an important parameter though, so you should
#' visually inspect your spectra before assigning it.
#' Typical values in our own individual instruments were:
#' SVC ~ c(990, 1900),
#' ASD ~ c(1001, 1801).
#'
#' If the factors used to match spectra are unreasonable, \code{match_sensors}
#' will throw. Unreasonable factors (f) are defined as 0.5 > f > 3 or NaN,
#' which happens when the value for the right sensor is 0.
#'
#' @param x spectra object
#' @param splice_at bands that serve as splice points, i.e the beginnings
#'                  of the rightmost sensor. Must be length 1 or 2 (max 3 sensors)
#' @param fixed_sensor sensor to keep fixed. Can be 1 or 2 if matching 2 sensors.
#'                     If matching 3 sensors, `fixed_sensor` must be 2 (default).
#' @param interpolate_wvl extent around splice_at values over which the splicing
#'                        factors will be calculated. Defaults to 5
#' @return spectra object
#'
#' @importFrom stats approx
#'
#' @author Jose Eduardo Meireles and Anna Schweiger
#' @export
#'
match_sensors = function(x,
                         splice_at,
                         fixed_sensor    = 2,
                         interpolate_wvl = c(5, 1)){
    UseMethod("match_sensors")
}


#' @describeIn match_sensors Match sensor overlap regions
#' @export
match_sensors.spectra = function(x,
                                 splice_at,
                                 fixed_sensor    = 2,
                                 interpolate_wvl = c(5, 1)){

    x            = x
    w            = bands(x)
    splice_at    = unlist(splice_at)
    fixed_sensor = ifelse( length(splice_at) == 2, 2, fixed_sensor)



    y = i_trim_sensor_overlap(x = x, splice_at = splice_at)
    x = y$spectra              # reassign x
    w = bands(x)               # reassign w
    s = split(w, y$sensor)

    interpolate_wvl = rep(interpolate_wvl, length.out = length(splice_at))

    ## Pick bands by sensor to computer factors
    wl_picks = lapply(seq_along(splice_at), function(z){
        low   = splice_at[z] - interpolate_wvl[z]
        high  = splice_at[z] + interpolate_wvl[z]
        left  = s[[ z ]][ s[[ z ]]         >= low ]
        right = s[[z + 1L ]][ s[[z + 1L ]] <= high ]

        # solve issues if any of the picks are empty
        if(length(left)  == 0){
            left = max(s[[ z ]])
        }
        if(length(right) == 0){
            right = min(s[[ z + 1L ]])
        }

        list("left"  = left, "right" = right)
    })
    names(wl_picks) = splice_at

    ## compute splicing factors
    splice_factors = lapply(seq_along(wl_picks), function(z){
        y = setNames(c(z, z + 1), c("left", "right"))
        m = names(y[match(fixed_sensor, y)])

        if(m == "right"){
            fixed  = wl_picks[[z]]$left
            scaled = wl_picks[[z]]$right
        } else {
            fixed  = wl_picks[[z]]$right
            scaled = wl_picks[[z]]$left
        }

        rowMeans(value(x[ , scaled, simplify = FALSE])) /
        rowMeans(value(x[ ,  fixed, simplify = FALSE]))
    })

    ## Compute the factor matrices
    ## These functions need to be empirically derived. Current implementation
    ## is just a hack and should not be used in production code

    s[fixed_sensor] = NULL

    factor_mat = lapply(seq_along(splice_factors), function(z){
        wvl = s[[z]]
        fac = splice_factors[[z]]

        sapply(fac, function(q){
            approx(x = range(wvl), y = c(1, q), xout = wvl)$y
        })

    })

    ## Transform data
    # for(i in seq_along(factor_mat)){
    #     x[ , s[[i]]] = value(x[ , s[[i]] ] ) * t( factor_mat[[i]] )
    # }

    x[ , s[[1]] ] = value(x[ , s[[1]] ] ) * t( factor_mat[[1]] )
    x
}
