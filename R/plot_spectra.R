#' Plot SVC spectra
#'
#' Function to plot SVC spectra from a data.frame
#'
#' @param dat A data.frame, as produced by read.svc. The measurements
#' need to be per row. The filename, usually the first column should be
#' exluded to avoid error message.
#' @param first Numeric. The first spectrum to plot. Corresponding to a row in a data.frame.
#' @param last Numeric. The last spectrum to plot. Corresponding to a row in a data.frame.
#' @return The output is a plot.
#' @examples
#' \dontrun{
#' plot.svc(myspectra[,-1], 1, 10)
#' }


###### 10-18-2016 ##########################

######## Plot spectra ###########
#### NOTE: run read.svc first ####
#### read.svc(inputdir = inputdir,read=T, filename = F)
#### TODO functionality for removing "X" when dataframe is read directly

plot.spectra.svc <- function (dat, first, last) {
    plot(as.numeric(names(dat)), dat[1,1:ncol(dat)],ylim=c(0,0.6),
         type = "n",xlab="wavelength", ylab="reflectance")
    for (i in first:last) {
      lines(as.numeric(names(dat)), dat[i,1:ncol(dat)], col=i)
    }
}


