#' Plot spectra
#'
#' @param spec spectra object
#' @param ylab label for y axis. defaults to "Reflectance"
#' @param xlab label for x axis. defaults to "Wavelength"
#' @param lty line type. defaults to 2
#' @param col line color. defaults to "black"
#' @param ... other arguments passed to matplot
#'
#' @return nothing. called for side effect
#' @export
plot.spectra = function(spec,
                        ylab = "Reflectance",
                        xlab = "Wavelength",
                        col  = "black",
                        lty  = 2,
                        type = "l",
                        ...) {

    # if(type != "l" || type != "n"){
    #     warning("spectra plot is likely to work best with type = 'l' or no plotting, i.e. type = 'n' ")
    # }

    matplot(x    = spec$wavelengths,
            y    = t(spec$reflectance),
            type = type,
            ylab = ylab,
            xlab = xlab,
            lty  = lty,
            col  = col, ...)
}


#' Plot plogygon for spectra quantiles
#'
#' @param spec Spectra object
#' @param total_prob Total mass to encompass. Number betwen 0.0 and 1.0
#' @param col Polygon color
#' @param add If add = FALSE (default), a new plot ir created. Otherwise
#'            (add = T), the polygon is added to the current plot.
#' @param ... Other parameters passed to polygon
#'
#' @return nothing. called for its side effect
#' @export
plot_quantile = function(spec,
                         total_prob = 0.95,
                         col        = rgb(0, 0, 0, 0.1),
                         add        = FALSE,
                         ...){

    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    if(total_prob < 0.0 || total_prob > 1.0){
        stop("total_prob must be between 0.0 and 1.0")
    }

    tail_mag   = (1.0 - total_prob) / 2.0
    tail_range = c(min = 0.0 + tail_mag,
                   max = 1.0 - tail_mag )

    qt = quantile(spec, probs = tail_range)
    xx = c( qt$wavelengths, rev(qt$wavelengths) )
    yy = c( qt$reflectance[1, ], rev(qt$reflectance[2, ]) )

    if(!add){
        plot(spec, type = "n")
    }

    polygon(x = xx, y = yy, col = col, ...)
}
