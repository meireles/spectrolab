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

    # if(length(d) > 1){
    #     stop("can only deal with a single duplicate.")
    # }

    # i      = which(w == d)
    # idx_rm = ifelse(d > boundary, i[1], i[2])


    if(length(d) > 1){
        message("Found more than one duplicated wavelength.\nInspect the spectra before further analyses.")
    }

    idx_rm = sapply(d, function(x){
        i = which(w %in% x)
        ifelse(x > boundary, i[1], i[2])
    })

    ## HACK. There is no easy way of subsetting wavelengths if they are
    ## duplicated. Therefore, I have to change the value of the wavelength
    ## I want to exclude and then remove that.

    ## Assign a dummy wavelength value to the wl to rm
    bogus                    = 12345678911121110987654321.0123
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
#' splice_at has no default because sensor transition points vary between vendors
#' and individual instruments. It is an important parameter though, so you should
#' visually inspect your spectra before assigning it.
#' Typical values in our own individual instruments were:
#' SVC ~ c(990, 1900)
#' PSR ~ c()
#' ASD ~ c(1001, 1801)
#'
#' If the factors used to match spectra are unreasonable, \code{match_sensors}
#' will throw. Unreasonable factors (f) are defined as 0.5 > f < 1.5 or NaN,
#' which  happens when the reflectance value for the right sensor is 0.
#'
#' @param x spectra object
#' @param splice_at wavelengths that serve as splice points Typically the
#'                  beginnings of sensor 2 and sensor 3.
#' @param interpolate_wvl blah
#' @param factor_range range of acceptable correction factors (min, max).
#'                     Defaults to c(0.5, 2)
#' @return spectra object
#'
#' @author Jose Eduardo Meireles and Anna Schweiger
#' @export
match_sensors = function(x, splice_at, interpolate_wvl = 5, factor_range = c(0.5, 2)){
    UseMethod("match_sensors")
}


#' @describeIn match_sensors Match sensor overlap regions
#' @export
match_sensors.spectra = function(x, splice_at, interpolate_wvl = 5, factor_range = c(0.5, 2)){

    message("Warning: feature under development!")
    message("match_sensors: should not be used in poduction code.")
    message("match_sensors: API will change.")

    w = wavelengths(x)

    if( ! i_is_increasing(x = w, stop = FALSE) ){
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
    p1  = s$sensor_1[ s$sensor_1 >= splice_at[1] - interpolate_wvl[1] ]
    p21 = s$sensor_2[1]
    p23 = s$sensor_2[ length((s$sensor_2)) ]
    p3  = s$sensor_3[ s$sensor_3 < splice_at[2] + interpolate_wvl[2] ]

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

    # f1_nan     = which(is.nan(f1))
    # f3_nan     = which(is.nan(f3))
    # f1_outside = which( f1 < factor_min | f1 > factor_max )
    # f3_outside = which( f3 < factor_min | f3 > factor_max )
    #
    # if(length(f1_nan) > 0){
    #     stop("Conversion factor to match sensors 1 and 2 could not be computed for spectra: ", paste(f1_nan, sep = " ") )
    # }
    # if(length(f3_nan) > 0){
    #     stop("Conversion factor to match sensors 2 and 3 could not be computed for spectra: ", paste(f3_nan, sep = " ") )
    # }
    #
    # if(length(f1_outside) > 0 ){
    #     warning("Conversion factors to match sensors 1 and 2 are outside of reasonable values for spectra: ",
    #             paste(f1_outside, sep = " "))
    # }
    #
    # if(length(f3_outside) > 0){
    #     warning("Conversion factors to match sensors 2 and 3 are outside of reasonable values for spectra: ",
    #             paste(f3_outside, sep = " "))
    # }

    f1_out_or_nan = which( f1 < factor_range[[1]] | f1 > factor_range[[2]] | is.nan(f1))
    f3_out_or_nan = which( f3 < factor_range[[1]] | f3 > factor_range[[2]] | is.nan(f3))

    if(length(f1_out_or_nan) > 0 ){
        stop("Conversion factors to match sensors 1 and 2 are outside of reasonable values for spectra: ",
                paste(f1_out_or_nan, sep = " "))
    }

    if(length(f3_out_or_nan) > 0){
        stop("Conversion factors to match sensors 2 and 3 are outside of reasonable values for spectra: ",
                paste(f3_out_or_nan, sep = " "))
    }

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
