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


#' Guess splice bands (bounds between senors)
#'
#' @param x spectra object
#' @return vector of band values
#'
#' @author Jose Eduardo Meireles
#' @export
guess_splice_at = function(x){
    UseMethod("guess_splice_at")
}


#' @describeIn guess_splice_at Guess splice bands (bounds between senors)
#' @export
guess_splice_at.spectra = function(x){

    w         = bands(x)
    b         = i_find_sensor_overlap_bounds(w, idx = FALSE)
    nb        = ncol(b)

    if(nb > 1){

        # If there is an overlap (i.e. SVC or PSR)

        splice_at = rep(NA, nb - 1)

        for(i in seq(nb - 1)){
            scalar       = 2 * i # Use most of the right sensor instead of the left one
            splice_at[i] = (b[1, i + 1] * scalar + b[2, i]) / (scalar + 1)
        }

    } else {

        message("Guessing sensor bounds is unreliable so please visually inspect your spectra.")

        # If there is no overlap (e.g. ASD), you may see a "jump" between adjacent
        # bands.

        # Trim ends
        p = 0.10             # percent to trim
        r = range(bands(x))
        t = diff(r) * p
        y = x[ , bands(x, r[1] + t, r[2] - t)]

        # Take the third derivative of the spectra and calculate its average
        # by band
        z = apply(y, 1, diff, difference = 3)
        e = rowMeans(abs(z))

        # Find the band names that exceeds x SDs from the mean
        s = 6
        m = mean(e) + s * sd(e)
        e = e[ e >= m ]
        g = sort(as.numeric(names(e)))

        # Now there are "clusters" of band names that meed that the mean + x SD
        # criterion. I will assume that sensors are not more than j units (e.g. nm)
        # apart so I can return the smallest band name from each cluster

        j         = 100
        splice_at = g[ c(TRUE, diff(g) >= j) ]
    }
    return(splice_at)
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

    w         = bands(x)
    b         = i_find_sensor_overlap_bounds(w)
    bb        = b
    no_over   = ncol(b) == 1
    splice_at = sort(splice_at)

    if(no_over){

        # If no overlap is found, break by splice_at since match_spectra
        # assumes that the data will be split

        b = matrix(NA, nrow = 2, ncol = length(splice_at) + 1)
        b[[ 1 ]]            = 1
        b[[ prod(dim(b)) ]] = length(w)

        rownames(b) = c("begin", "end")
        colnames(b) = paste0("sensor_", seq(ncol(b)))


        for(i in 1:length(splice_at)){
            m = max(which(w <= splice_at[i]))
            b[1, i + 1] = m
            b[2 , i]    = m - 1
        }

        b = data.frame(b)

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
         "sensor"  = rep(names(s), sapply(s, length)),
         "overlap" = ifelse(no_over, NA, bb))
}


#' Match spectra at sensor transitions
#'
#' \code{match_sensors} scales values of sensors 1 (VIS) and 3 (SWIR 2)
#'
#' Splice_at has no default because sensor transition points vary between vendors
#' and individual instruments.
#' The function \code{guess_splice_at} can help you guess what those values could
#' be. However, \code{splice_at} is an important parameter though, so you should
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
                                 interpolate_wvl = c(5, 2)){

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
            fixed  = wl_picks[[z]]$right
            scaled = wl_picks[[z]]$left
        } else {
            fixed  = wl_picks[[z]]$left
            scaled = wl_picks[[z]]$right
        }

        rowMeans(value(x[ ,  fixed, simplify = FALSE])) /
        rowMeans(value(x[ , scaled, simplify = FALSE]))

    })

    ## Compute the factor matrices
    ## These functions need to be empirically derived. Current implementation
    ## is just a hack and should not be used in production code

    s[fixed_sensor] = NULL

    factor_mat = lapply(seq_along(splice_factors), function(z){
        wvl = s[[z]]
        fac = splice_factors[[z]]

        y   = setNames(c(z, z + 1), c("left", "right"))
        m   = names(y[match(fixed_sensor, y)])

        if(m == "right"){
            r =  sapply(fac, function(q){
                approx(x = range(wvl), y = c(1, q), xout = wvl)$y
            })
        } else {
            r =  sapply(fac, function(q){
                approx(x = range(wvl), y = c(q, 1), xout = wvl)$y
            })
        }
        r
    })


    if(is.na(y$overlap) | length(factor_mat) == 1){
        iter = seq_along(factor_mat)
    } else {
        iter = 1
    }

    ## Transform data
    for(i in iter){
        x[ , s[[i]]] = value(x[ , s[[i]] ] ) * t( factor_mat[[i]] )
    }

    x
}
