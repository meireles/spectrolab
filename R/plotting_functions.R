devtools::use_package("shiny")
devtools::use_package("shinyjs")
devtools::use_package("RColorBrewer")

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
#'
#' @examples
#' library(spectrolab)
#' spec  = as.spectra(spec_matrix_example)
#' plot(spec, lwd = 1.2)
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
#' @param na.rm boolean. remove NAs to compute quantiles? Defaults to TRUE
#' @param ... other parameters passed to polygon() or to plot.
#' @return nothing. Called for its side effect.
#'
#' @importFrom graphics polygon
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec  = as.spectra(spec_matrix_example)
#' plot_quantile(spec, total_prob = 0.5)
plot_quantile = function(spec,
                         total_prob = 0.95,
                         col        = rgb(0, 0, 0, 0.1),
                         border     = TRUE,
                         add        = FALSE,
                         na.rm      = TRUE,
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

    qt = quantile(spec, probs = tail_range, na.rm = na.rm)
    # xx = c( qt$wavelengths,
    #         rev(qt$wavelengths) )
    # yy = c( qt$reflectance[1, ], rev(qt$reflectance[2, ]) )

    xx = c(wavelengths(qt),
           rev(wavelengths(qt)))
    yy = c(reflectance(qt)[1, ],
           rev( reflectance(qt)[2, ]))

    if(!add){
        plot(spec, type = "n", ...)
    }

    graphics::polygon(x = xx, y = yy, col = col, border = border, ...)
}


#' Return default spectral regions matrix
#'
#' @return matrix with default_spec_regions
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' # matrix that defines regions on the spectra
#' # Useful for plotting w/ plot_regions()
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
#' @param add_label boolean. Add region column names on top of the polygons?
#' @param cex_label label scale
#' @param ... additional parameters passed to polygon().
#' @return nothing. Called for its side effect.
#'
#' @details
#' Default regions:
#' spec_regions = cbind("VIS"   = c(begin = 400,  end = 700),
#'                      "NIR"   = c(begin = 800,  end = 1300),
#'                      "SWIR1" = c(begin = 1550, end = 1800),
#'                      "SWIR2" = c(begin = 2000, end = 2400)).
#'
#' @importFrom grDevices rgb
#' @importFrom graphics mtext par polygon
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
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
plot_regions = function(spec,
                        regions   = default_spec_regions(),
                        col       = grDevices::rgb(0.7, 0.7, 0.7, 0.3),
                        border    = FALSE,
                        add       = TRUE,
                        add_label = TRUE,
                        cex_label = 1,
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

    if(add_label){
        region_txt = colnames(m_regions)
        region_pos = colMeans(m_regions)

        # scale mtext
        cex  = graphics::par("cex.axis")
        nc   = graphics::par("mfrow")[2]
        brks = c(1, 2, 3, 4, 1000)
        r    = findInterval(nc, brks)
        m    = c(1.0, 0.86, 0.68, 0.60)[r]

        # plot margin text
        graphics::mtext(region_txt, side = 3, at = region_pos, cex = cex * m * cex_label)
    }
}

#' Plot spectra interactively
#'
#' Iteratively plots spectra with a shiny app. Useful to inspect large datasets.
#'
#' \code{plot_interact} limits the number of spectra displayed at once to 600 for
#' performance reasons. As of now, the function does not return anything and does
#' not have side effects. This means that spectra can be selected and highlighted
#' but not yet deleted or subset from the shiny app.
#'
#' @param spec spectra object
#' @param colpalette a color palette function, e.g. rainbow, terrain.colors, or a
#'                   function returned by colorRampPalette() or colorRamps package
#' @param ... Other arguments passed to plot
#' @return interactive plot
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats dist
#'
#' @author Anna K. Schweiger and Jose Eduardo Meireles
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a spectra object
#' spec = as.spectra(spec_matrix_example)
#'
#' # Start interactive plot
#' plot_interactive(spec)
#' }
plot_interactive = function(spec,
                            colpalette = function(n) RColorBrewer::brewer.pal(n, "Dark2"),
                            ... ){
    if (! requireNamespace("shiny", quietly = TRUE)) {
        stop("Package 'shiny' needed for this function to work. Please install it.",
             call. = FALSE)
    }

    if( ! is.function(colpalette) ){
        message("colpalette must be a function! Using the default palette.")
        colpalette = function(n) RColorBrewer::brewer.pal(n, "Dark2")
    }

    # Constants
    n_max     = nrow(spec)
    i_display = min(10,  n_max)  # Initial display = 10
    m_display = min(600, n_max)  # Maximum display = 600
    wvl_min   = min(spectrolab::wavelengths(spec))
    wvl_max   = max(spectrolab::wavelengths(spec))

    # Should be variables
    band_threshold = 3
    refl_threshold = 0.03

    # Find spectral distances
    spec_dist = t(as.matrix(spec)) - as.vector(as.matrix(mean(spec)))
    spec_dist = sqrt(colSums(spec_dist^2))

    # find order of magnitude of spec_dist
    dist_mag  = ceiling( log10( 1 / min(diff(sort(spec_dist))) ) )

    # and round it
    spec_dist = round(spec_dist, dist_mag)

    # Begin shiny app
    shiny::shinyApp(
        ui = shiny::fluidPage(
            shinyjs::useShinyjs(),
            shiny::titlePanel("spectrolab"),
            shiny::fluidRow(
                shiny::column(3,
                              shiny::wellPanel(
                                  shiny::h5(shiny::textOutput("firstlast")),
                                  shiny::h5(shiny::textOutput("selected"))
                              ),
                              shiny::wellPanel(
                                  shiny::numericInput(inputId = "n_display",
                                                      label   = "display number",
                                                      value   = i_display,
                                                      min     = 1,
                                                      max     = m_display,
                                                      width   = "100%"),
                                  shiny::actionButton("go_back", label = "previous", width = "45%"),
                                  shiny::actionButton("go_fwd",  label = "next", width = "45%"),
                                  shiny::checkboxInput(inputId = "highlight_by_dist",
                                                       label   = "color by distance",
                                                       value   = FALSE, width = "100%"),
                                  shiny::sliderInput(inputId = "dist_highlight",
                                                     label   = "distance from mean",
                                                     min     = min(spec_dist),
                                                     max     = max(spec_dist),
                                                     value   = mean(spec_dist),
                                                     sep     = "",
                                                     ticks   = TRUE,
                                                     width   = "100%")
                              )
                ),
                shiny::column(9,
                              align = "center",
                              shiny::plotOutput("spectrum",
                                                width = "100%",
                                                click = "plot_click"),
                              shiny::sliderInput(inputId = "w_range",
                                                 label   = "Wavelengths",
                                                 min     = wvl_min,
                                                 max     = wvl_max,
                                                 value   = c(wvl_min, wvl_max),
                                                 ticks   = TRUE,
                                                 width   = "100%")
                )
            )
        ),

        server = function(input, output, session){
            # Initialize range variables
            from    = shiny::reactiveVal(1)
            to      = shiny::reactiveVal(1)

            # Initialize highlighted index
            picked = shiny::reactiveVal(NULL)

            # Update `from`, `to` and `picked` if next is pressed
            shiny::observeEvent(input$go_fwd, {
                if(to() < n_max){
                    # update from
                    old_from = from()
                    new_from = min(old_from + input$n_display, n_max)
                    from(new_from)

                    # update to
                    new_to   = min(from() + input$n_display - 1L, n_max)
                    to(new_to)

                    # update picked
                    picked(NULL)
                }
            })

            # Update `from`, `to` and `picked` if previous is pressed
            shiny::observeEvent(input$go_back ,{
                # update from
                old_from = from()
                new_from = max(old_from - input$n_display, 1L)
                from(new_from)

                # update to
                new_to = min(from() + input$n_display - 1L, n_max)
                to(new_to)

                # update picked
                picked(NULL)
            })

            # Update `to` and `picked` if n_display is changed
            shiny::observeEvent(input$n_display ,{

                if(input$n_display > m_display){
                    updateNumericInput(session, "n_display", value = m_display)
                }

                new_to   = min(from() + input$n_display - 1L, n_max)

                if(!is.null(picked())){
                    if(picked() > new_to){
                        picked(NULL)
                    }
                }

                to(new_to)
            })

            shiny::observeEvent(input$highlight_by_dist, {
                shinyjs::toggleState("dist_highlight")
            })

            # Update picked spec
            shiny::observeEvent(input$plot_click ,{
                click_coord  = input$plot_click
                bands        = spectrolab::wavelengths(spec) # probably should filter by w_range
                bands_diff   = abs(bands - click_coord[[1]])
                band_clicked = bands[ which(bands_diff == min(bands_diff) & bands_diff <= band_threshold) ]

                refl         = spec[ seq(from(), to()), band_clicked]
                refl_diff    = abs(refl -  click_coord[[2]])
                spec_clicked = which(refl_diff == min(refl_diff) & refl_diff <= refl_threshold)

                if(length(spec_clicked) == 0){
                    picked(NULL)
                } else {
                    spec_clicked = from() + spec_clicked - 1L
                    picked(spec_clicked)
                }
            })

            # Plot spectra
            output$spectrum = shiny::renderPlot({
                s_range = seq(from(), to())
                w_range = spectrolab::wavelengths(spec, min(input$w_range), max(input$w_range))

                cols = if(input$highlight_by_dist == TRUE){
                    ifelse(spec_dist[s_range] > input$dist_highlight, "orange", "black")
                } else {
                    suppressWarnings( colpalette(length(s_range)) ) ## suppressWarnings
                }

                plot(spec[s_range, w_range], col = cols, ...)

                if( ! is.null(picked()) ){
                    plot(spec[picked(), ], col = "red", lwd = 2, add = TRUE)
                }
            })

            # Show range of spectra
            output$firstlast = shiny::renderText({
                paste("Spectra: ", from(), "-", to(), "/", n_max, sep = "")
            })

            # Show selected spectra
            output$selected = shiny::renderText({
                if(!is.null(picked())){
                    selected = picked()
                } else {
                    selected = "none"
                }
                paste("Selected: ", selected, sep = "")
            })
        }
    )
}
