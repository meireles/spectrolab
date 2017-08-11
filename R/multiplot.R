devtools::use_package("shiny")
devtools::use_package("grDevices")

#' Interactive spectra plot
#'
#' \code{plot_interact} iteratively plots a defined number of spectra. This is
#' helpful for data inspection and large datasets.
#'
#' @param spec spectra object
#' @param colpalette a color palette function, e.g. rainbow, terrain.colors, etc.
#'                   or a function returned from `colorRampPalette`.
#' @param ... Other arguments passed to plot
#' @return interactive plot
#'
#' @examples
#' \dontrun{
#' # Create a spectra object
#' spec = as.spectra(spec_matrix_example)
#'
#' # Start interactive plot
#' plot_interact(spec)
#' }
#'
#' @importFrom shiny shinyApp numericInput actionButton verbatimTextOutput plotOutput renderPlot renderText
#' @importFrom grDevices colorRampPalette rainbow heat.colors terrain.colors
#'
#' @author Anna K. Schweiger and Jose Eduardo Meireles
#' @export

plot_interact = function(spec,
                         colpalette = grDevices::colorRampPalette(c("red", "orange", "blue", "purple")),
                         ... ){

    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Package shiny needed for this function to work. Please install it.",
             call. = FALSE)
    }

    if( ! is.function(colpalette) ){
        message("colpalette must be a function! Using the default palette.")
        colpalette = grDevices::colorRampPalette(c("red", "orange", "blue", "purple"))
    }

    # Constants
    n_max     = nrow(spec)
    i_display = min(10, n_max)

    # Begin shiny app
    shiny::shinyApp(
        ui = shiny::fluidPage(
            shiny::numericInput(inputId = "n_display",
                                label   = "number of spectra",
                                value   = i_display,
                                min     = 1,
                                max     = n_max,
                                width   = "20%"),
            shiny::actionButton("go_back", label = "previous"),
            shiny::actionButton("go_fwd", label = "next"),
            shiny::verbatimTextOutput("firstlast"),
            shiny::plotOutput("spectrum")
        ),

        server = function(input, output){
            # Initialize range variables
            from  = shiny::reactiveVal(1)
            to    = shiny::reactiveVal(1)

            # Update `from` and `to` if next is pressed
            shiny::observeEvent(input$go_fwd, {
                # update from
                old_from = from()
                new_from = min(old_from + input$n_display, n_max)
                from(new_from)

                # update to
                new_to   = min(from() + input$n_display - 1L, n_max)
                to(new_to)
            })

            # Update `from` and `to` if previous is pressed
            shiny::observeEvent(input$go_back ,{
                # update from
                old_from = from()
                new_from = max(old_from - input$n_display, 1L)
                from(new_from)

                # update to
                new_to = min(from() + input$n_display - 1L, n_max)
                to(new_to)
            })

            # Update `to` if n_display is changed
            shiny::observeEvent(input$n_display ,{
                new_to   = min(from() + input$n_display - 1L, n_max)
                to(new_to)
            })

            # Plot spectra
            output$spectrum = shiny::renderPlot({
                s_range = seq(from(), to())
                plot(spec[ s_range, ], col = colpalette(length(s_range)), ...)
            })

            # Plot text
            output$firstlast = shiny::renderText({
                paste("spectra: ", from(), "-", to(), "/", n_max, sep = "")
            })
        }
    )
}
