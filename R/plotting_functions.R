#' Plot spectra
#'
#' \code{plot} plots spectra.
#'
#' @param x spectra object
#' @param ylab label for y axis. Defaults to "Reflectance".
#' @param xlab label for x axis. Defaults to "Wavelength".
#' @param col line color. Defaults to "black".
#' @param lty line type. Defaults to 1.
#' @param type type of plot. Meant to take either line "l" or no plotting "n".
#' @param ... other arguments passed to matplot.
#' @return nothing. Called for side effect.
#'
#' @importFrom graphics matplot plot
#'
#' @author Jose Eduardo Meireles
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

    ## Note on importFrom:
    ## Also using "@importFrom graphics plot" because a generic plot is not declared
    ## anywhere, but graphics::plot is not directly called by this method.

    graphics::matplot(x    = wavelengths(x),
                      y    = t(reflectance(x)),
                      type = type,
                      ylab = ylab,
                      xlab = xlab,
                      lty  = lty,
                      col  = col,
                      ...)
}

#' Plot spectra quantiles
#'
#' \code{plot_quantile} plots polygons for the quantiles of spectra per wavelength.
#'
#' @param spec spectra object
#' @param total_prob total probability mass to encompass. Single number
#'                   between 0.0 and 1.0. Defaults to 0.95.
#' @param col polygon color
#' @param border boolean. Draw border?
#' @param add if add = FALSE (default), a new plot is created. Otherwise
#'            (add = TRUE), the quantile is added to the current plot.
#' @param ... other parameters passed to polygon().
#' @return nothing. Called for its side effect.
#'
#' @importFrom graphics polygon
#'
#' @author Jose Eduardo Meireles
#' @export
plot_quantile = function(spec,
                         total_prob = 0.95,
                         col        = rgb(0, 0, 0, 0.1),
                         border     = TRUE,
                         add        = FALSE,
                         ...){

    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    if( ! is.vector(total_prob) || length(total_prob) != 1 ){
        stop("total_prob must be a single number")
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

    graphics::polygon(x = xx, y = yy, col = col, border = border, ...)
}


#' Return default spectral regions matrix
#'
#' @return matrix with default_spec_regions
#'
#' @author Jose Eduardo Meireles
#' @export
default_spec_regions = function(){
    cbind("VIS"   = c(begin = 400,  end = 700),
          "NIR"   = c(begin = 800,  end = 1300),
          "SWIR1" = c(begin = 1550, end = 1800),
          "SWIR2" = c(begin = 2000, end = 2400))

}


#' Plot polygons for spectral regions
#'
#' \code{plot_regions} plots polygons for default (VIS, NIR, SWIR 1, SWIR 2) or customized regions of the spectrum.
#'
#' @param spec spectra object
#' @param regions matrix with spectral regions in columns and only two rows named
#'                "begin" and "end". Values are the wavelengths where a spectral
#'                regions begins and ends. See details for how the default regions are defined.
#' @param col color for regions. Single value or vector of length ncol (regions).
#' @param border color for region borders. Defaults to FALSE (no border).
#' @param add boolean. If TRUE (default) adds polygons to current plot (if a plot
#'            exists) or throws an error if a plot doesn't exist.
#'            If FALSE, a new plot is created **without** any spectra.
#' @param names boolean. Add region column names on top of the polygons?
#' @param ... additional parameters passed to polygon().
#' @return nothing. Called for its side effect.
#'
#' @details
#' Default regions:
#' spec_regions = cbind("VIS"   = c(begin = 400,  end = 700),
#'                      "NIR"   = c(begin = 800,  end = 1300),
#'                      "SWIR1" = c(begin = 1550, end = 1800),
#'                      "SWIR2" = c(begin = 2000, end = 2400)).
#' @examples
#'
#' spec = as.spectra(spec_matrix_example)
#' plot_regions(spec, default_spec_regions())
#' plot(spec, add = TRUE)
#'
#' # Alternatively, if you want to get fancy...
#' \dontrun{
#' col_fun = colorRampPalette(c(rgb(1, 1, 0, 0.7),rgb(1, 0, 0, 0.7)), alpha = TRUE)
#' colors = col_fun(4)
#'
#' plot_regions(spec,default_spec_regions(), col = colors)
#' plot(spec, add = TRUE)
#' }
#'
#' @importFrom grDevices rgb
#' @importFrom graphics mtext par polygon
#'
#' @author Jose Eduardo Meireles
#' @export
plot_regions = function(spec,
                        regions = default_spec_regions(),
                        col     = grDevices::rgb(0.7, 0.7, 0.7, 0.3),
                        border  = FALSE,
                        add     = TRUE,
                        names   = TRUE,
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

    xx_mat = m_regions[ c("begin", "begin", "end", "end"),  , drop = FALSE]
    yy_mat = i_plot_boundaries(return_mat = TRUE)
    yy_vec = yy_mat[ c("min", "max", "max", "min") , "y"]

    if(!add){
        plot(spec, type = "n")
    }

    if( (!i_plot_exists()) && add) {
        warning("No plot exists for `regions` to be added to, but `add` is set to TRUE.\n  Plotting regions anyways." )
        plot(spec, type = "n")
    }

    for(i in 1:ncol(xx_mat)) {
        graphics::polygon(xx_mat[ , i], yy_vec, col = col[i], border = border, ...)
    }

    if(names){
        region_txt = colnames(m_regions)
        region_pos = colMeans(m_regions)

        # scale mtext
        cex  = graphics::par("cex.axis")
        nc   = graphics::par("mfrow")[2]
        brks = c(1, 2, 3, 4, 1000)
        r    = findInterval(nc, brks)
        m    = c(1.0, 0.86, 0.68, 0.60)[r]

        # plot margin text
        graphics::mtext(region_txt, side = 3, at = region_pos, cex = cex * m)
    }
}
