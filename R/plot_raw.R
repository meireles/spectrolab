#' Plot unprocessed spectra
#'
#' Function to plot unprocessed SVC spectra from a data.frame
#'
#' @param dat A data.frame, e.g. as produced by read_spec. The measurements
#' need to be per row. The filename, usually the first column should be
#' exluded to avoid warning.
#' @param first Numeric. The first spectrum to plot. Corresponding to a row in a data.frame.
#' @param last Numeric. The last spectrum to plot. Corresponding to a row in a data.frame.
#' @return The output is a plot.
#' @examples
#' \dontrun{
#' plot_rawspec(Acer_spectra[,-1], 1, 10)
#' }
#' @export


###### 10-18-2016 ##########################

######## Plot spectra ###########
#### NOTE: run read_svc first ####åå

plot_rawspec <- function (dat, first, last) {
    plot(as.numeric(names(dat)), dat[1,1:ncol(dat)],ylim=c(0,0.6),
         type = "n",xlab="wavelength", ylab="reflectance")
    for (i in first:last) {
      lines(as.numeric(names(dat)), dat[i,1:ncol(dat)], col=i)
    }
}


