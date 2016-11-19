#' Plot spectra
#'
#' @param x spectra object
#' @param ylab label for y axis. defaults to "Reflectance"
#' @param xlab label for x axis. defaults to "Wavelength"
#' @param col line color. defaults to "black"
#' @param lty line type. defaults to 1
#' @param type type of plot. meant to take either line "l" or no plotting "n"
#' @param ... other arguments passed to matplot
#'
#' @return nothing. called for side effect
#' @export
plot.spectra = function(x,
                        ylab = "Reflectance",
                        xlab = "Wavelength",
                        col  = "black",
                        lty  = 1,
                        type = "l",
                        ...){

    if( ! type %in% c("l", "n")){
        warning("spectra plot is likely to work best with type = 'l' or no plotting, i.e. type = 'n' ")
    }

    matplot(x    = wavelengths(x),
            y    = t(reflectance(x)),
            type = type,
            ylab = ylab,
            xlab = xlab,
            lty  = lty,
            col  = col,
            ...)
}

#' Plot plogygon for spectra quantiles
#'
#' @param spec Spectra object
#' @param total_prob Total mass to encompass. Number betwen 0.0 and 1.0
#' @param col Polygon color
#' @param add If add = FALSE (default), a new plot is created. Otherwise
#'            (add = T), the quantile is added to the current plot.
#' @param ... Other parameters passed to polygon()
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


#' return default spectral regions matrix
#'
#' @return matrix with default_spec_regions
#' @export
default_spec_regions = function(){
    cbind("VIS"   = c(begin = 400,  end = 700),
          "NIR"   = c(begin = 800,  end = 1300),
          "SWIR1" = c(begin = 1550, end = 1800),
          "SWIR2" = c(begin = 2000, end = 2400))

}


#' Plot polygons for spectral regions
#'
#' spec_regions = cbind("VIS"   = c(begin = 400,  end = 700),
#'                      "NIR"   = c(begin = 800,  end = 1300),
#'                      "SWIR1" = c(begin = 1550, end = 1800),
#'                      "SWIR2" = c(begin = 2000, end = 2400))
#'
#'
#' @param spec spectra object
#' @param regions matrix with spectral regions in columns and only two rows named
#'                "begin" and "end". Values are the wavelengths where a spectral
#'                regions begins and ends. See details for an example
#' @param col color for regions. single value or vector of length ncol(regions)
#' @param border color for region borders. Defaults to FALSE (no border)
#' @param add boolean. If TRUE (default) adds polygons to current plot. Otherwise
#'            a new plot is created **without** any spectra.
#' @param names boolean. add region column names on top of the polygons?
#' @param ... additional parameters passed to polygon()
#'
#' @return nothing. called for its side effect
#' @export
plot_spec_regions = function(spec,
                             regions,
                             col    = rgb(0.7, 0.7, 0.7, 0.3),
                             border = FALSE,
                             add    = FALSE,
                             names  = TRUE,
                             ...){
    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    m_regions = as.matrix(regions)

    if( any(rownames(m_regions) != c("begin", "end")) ){
        stop("regions matrix must have two rows named 'begin' and 'end'.")
    }

    if(length(col) != ncol(m_regions)){
        col = rep(col, length.out = ncol(m_regions))
    }

    xx_mat = m_regions[ c("begin", "begin", "end", "end"),  ]
    yy_mat = i_plot_boundaries(return_mat = TRUE)
    yy_vec = yy_mat[ c("min", "max", "max", "min") , "y"]

    if(!add){
        plot(spec, type = "n")
    }

    for(i in 1:ncol(xx_mat)) {
        polygon(xx_mat[ , i], yy_vec, col = col[i], border = border, ...)
    }

    if(names){
        region_txt = colnames(m_regions)
        region_pos = colMeans(m_regions)

        # scale mtext
        cex  = par("cex.axis")
        nc   = par("mfrow")[2]
        brks = c(1, 2, 3, 4, 1000)
        r    = findInterval(nc, brks)
        m    = c(1.0, 0.86, 0.68, 0.60)[r]

        # plot margin text
        mtext(region_txt, side = 3, at = region_pos, cex = cex * m)
    }
}
