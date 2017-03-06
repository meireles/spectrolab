#' Are wavelengths increasing
#'
#' \code{i_test_increasing_wavelengths} tests if wavelength values are increasing
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


#' Remove duplicated wavelength
#'
#' \code{i_remove_duplicated_wavelength} removes a duplicated wavelength
#'
#' SVC instruments usually have a certain wavelength that is repeated in the
#' overlap region of different sensors, e.g. 1005.5 nm. The choice of which
#' duplicate wavelength to remove (1005.5 in sensor 1 or sensor 2) will depend
#' on boundary. If the duplicated value is greater than boundary, the first
#' sensor duplicate is removed. Else, the second dup is removed.
#'
#' @param x spectra object
#' @param boundary boundary (double). If < wvl value, the first dup is pruned
#' @return spectra object
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_remove_duplicated_wavelength = function(x, boundary){
    w = wavelengths(x)
    d = w[ which(duplicated(w)) ]

    if(length(d) > 1){
        stop("can only deal with a single duplicate.")
    }

    i      = which(w == d)
    idx_rm = ifelse(d > boundary, i[1], i[2])

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
#' @param splice_at wavelengths where to splice sensors. suggests where the
#'                  begining of sensors 2 and 3 should be.
#' @return spectra object
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
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
#' \code{match_sensors} scales reflectance values of sensors 1 (vis) and 3 (swir2)
#'
#' splice_at has no default because sensor transition points vary bwteeen vendors
#' and individual instruments. It is an imporant parameter though, so you should
#' visually inspect your spectra before assigning it.
#' Typical values in our own individual instruments were:
#' SVC ~ c(990, 1900)
#' PSR ~ c()
#' ASD ~ c()
#'
#' @param x spectra object
#' @param splice_at wavelengths that serve as splice poits. Typically the
#'                  beginings of sensor 2 and sensor 3.
#' @param interpolate_wvl blah
#' @return spectra object
#'
#' @author Jose Eduardo Meireles and Anna Schweiger
#' @export
match_sensors = function(x, splice_at, interpolate_wvl = 5){
    UseMethod("match_sensors")
}


#' @describeIn match_sensors Match sensor ovelap regions
#' @export
match_sensors.spectra = function(x, splice_at, interpolate_wvl = 5){

    message("Warning: feature under development!")
    message("match_sensors: should not be used in poduction code.")
    message("match_sensors: API will change.")

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

    interpolate_wvl = rep(interpolate_wvl, length.out = length(splice_at))

    ## Pick wavelengths by sensor to computer factors
    p1 = s$sensor_1[ s$sensor_1 >= splice_at[1] - interpolate_wvl[1] ]
    p21 = s$sensor_2[1]
    p23 = s$sensor_2[ length((s$sensor_2)) ]
    p3 = s$sensor_3[ s$sensor_3 < splice_at[2] + interpolate_wvl[2] ]

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
        #seq(1.0, y, length.out = length(s$sensor_1))
        seq(y, y, length.out = length(s$sensor_1))
    })

    fm3 = sapply(f3, function(y){
        #seq(y, 1.0, length.out = length(s$sensor_3))
        seq(y, y, length.out = length(s$sensor_3))
    })

    ## Transform data
    x[ , s$sensor_1] = reflectance(x[ , s$sensor_1 ] ) * t(fm1)
    x[ , s$sensor_3] = reflectance(x[ , s$sensor_3 ] ) * t(fm3)

    x
}
