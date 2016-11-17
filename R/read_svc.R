
#' Read files from various formats into `spectra`
#'
#' @param path Path to directory or input files
#' @param format file formats. Currently the only option is "sig" (for SVC).
#' @param include_white_ref boolean. include white reference. NOT IMPLEMENTED YET
#' @param recursive read files recursively
#' @param exclude_if_matches exludes files that match this regular exptession.
#'                           Example: "BAD"
#' @param outside_01_action what to do with values outside 0 and 1? Options are
#'                          "NA", which replaces those values with NA or
#'                          "round", which sets negative values to 0 and >1 to 1.
#' @param ... nothing yet
#'
#' @return a single `spectra` or a list of `spectra` (in case files had diff number of wavelengths)
#' @export
read_spectra = function(path,
                          format,
                          include_white_ref  = FALSE,
                          recursive          = FALSE,
                          exclude_if_matches = NULL,
                          outside_01_action  = "round",  #"round" or "NA"
                          ...) {

    #########################################
    ## match formats
    #########################################
    format_lookup = c(sig = "sig", svc = "sig")
    format_match  = pmatch( tolower(format), names(format_lookup))

    ## Error if format isn't found
    if(length(format_match) == 0){ stop("Format not supported") }

    ## create regexpr to select files
    regexpr_ext = paste0("\\.", format_lookup[format_match], "$")

    #########################################
    ## validate paths
    #########################################
    i_path   = path
    f_exists = file.exists(i_path)
    is_dir   = dir.exists(i_path)

    ## Error if paths don't exist
    if(!all(f_exists)){ stop("Path not found:", i_path[!f_exists]) }

    ## Error if user mixed file names and dir names
    if(any(is_dir) && any(!is_dir)){ stop("Cannot mix directory and file paths!") }

    #########################################
    ## Read file names
    #########################################
    if( all(is_dir) ){                            ## if dir
        i_path = dir(path        = i_path,
                     pattern     = regexpr_ext,
                     full.names  = TRUE,
                     ignore.case = TRUE)
    } else {                                      ## if files
        m      = grep(regexpr_ext, i_path)
        i_path = i_path[m]
    }

    ## error if no files of the right extension are found
    if(length(i_path) == 0){stop("No file path matched", format_lookup[format_match])}

    #########################################
    ## filter files that match bad spectra
    #########################################
    if(!is.null(exclude_if_matches)){
        #bad_tag = paste0("\\.", exclude_if_matches, "$")
        bad_tag = exclude_if_matches
        m       = grepl(pattern = bad_tag, i_path)
        i_path  = i_path[!m]
    }

    ## error if bad spectra fiters out all file names
    if(length(i_path) == 0){stop("No paths left after removeing bad spectra. Check your `exclude_if_ends_with` param")}


    #########################################
    ## define behaviour refl outside 01
    #########################################
    if(outside_01_action == "round"){
        fix_out01 = function(x){
            x[ x < 0.0] = 0.0
            x[ x > 1.0] = 1.0
            x
            }
    } else if (outside_01_action %in% c("NA", "na", NA) ){
        fix_out01 = function(x){
            x[ x < 0.0 || x > 1.0] = NA
            x
        }
    } else {
        stop("outside_01_action must be either `round` or `NA`")
    }

    #############################################################
    ## Read spectra with the appropriate function
    #############################################################

    if(format_lookup[format_match] == "sig"){
        result = i_read_sig(i_path,
                            include_white_ref = include_white_ref,
                            outside_01_fun    = fix_out01)
        return(result)
    }

    # if my other instrumet, use function blah ...
}


#' Intenral parser for SVC .sig files
#'
#' @param file_paths paths for .sig files, already parsed by `read_spectra`
#' @param include_white_ref NOT USED YET, but should read the wite reference from each `.sig` file
#' @param outside_01_fun function to deal with reflectance values outside 0.1.
#' @param ... NOT USED YET
#'
#' @return single `spectra` file or list of `spectra`
i_read_sig = function(file_paths,
                      include_white_ref,
                      outside_01_fun,
                      ...){

    parse_sig = function(x) {
        ## wl in col 1 and refl in col 7
        result = read.delim(x, sep = " ", skip = 25, header = FALSE)[ , c(1, 7)]
        colnames(result) = c("wl", "refl")
        result
    }

    ## read sig data
    sig_data        = lapply(file_paths, parse_sig)
    names(sig_data) = file_paths

    ## there mabye files with different number of bands. check for them
    sig_ncol = unlist(lapply(sig_data, nrow))
    sig_data = split(sig_data, sig_ncol)

    ## Construct spectra
    spec = lapply(sig_data, function(x) {
        rf = lapply(x, function(y){ y[ , "refl"] })
        rf = do.call(rbind, rf)
        rf = rf / 100               # svc has reflectances from ~ 1 to 100.
        rf = outside_01_fun(rf)     # deal with values outside 01
        wl = x[[1]][ , "wl"]
        nm = basename(names(x))

        spectra(rf, wl, nm)
    })

    if(length(spec) > 1){
        warning("Returning a list of `spectra` beause some files\n
                had different number of bands.")
    } else {
        spec[[1]]
    }
}
