################################################################################
################################################################################
#' Interactive spectra plot
#'
#' \code{plot_interact} iteratively plots a defined number of spectra. This is
#' helpful for data inspection and large datasets.
#'
#' @param spec spectra object
#' @return interactive plot
#'
#' @examples
#' \dontrun{
#' # Create a reflectance matrix.
#' rf <- spec_matrix_example[, -1]
#'
#' # Create a vector with wavelength labels.
#' wl <- colnames(rf)
#'
#' # Sample with replacement to increase the number of spectra.
#' rf <- rf[sample(nrow(rf),300,replace = T),]
#'
#' # Create a vector with sample labels that match the reflectance matrix rows.
#' sn <- paste(rep("sp",300), 1:300, sep = "_")
#'
#' # Construct the spectra object using the `spectra` constructor
#' spec <- spectra(reflectance = rf, wavelengths = wl, names = sn)
#'
#' # Start interactive plot
#' plot_interact(spec)
#' }
#' @author Anna K. Schweiger
#' @export

plot_interact <- function(spec) {
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Package shiny needed for this function to work. Please install it.",
             call. = FALSE)
    }
    shiny::shinyApp(
        ui = shiny::fluidPage(
  shiny::numericInput(inputId = "num1", label= "number of spectra",value = 20, min = 1,
               width = "25%"),
  shiny::actionButton("action2", label = "previous"),
  shiny::actionButton("action", label = "next"),
  shiny::verbatimTextOutput("firstlast"),
  shiny::plotOutput("spectrum")
  ),

  server = function(input, output){
  output$spectrum <-  shiny::renderPlot({
    plot(spec[(1:input$num1)+input$num1*input$action-input$num1*input$action2,], col=1:50)
      })
  output$firstlast <- renderText({paste("spectra from",1+input$num1*input$action-input$num1*input$action2,
                                    "to", input$num1+input$num1*input$action-input$num1*input$action2)})
  }
    )
    }



