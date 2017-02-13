devtools::use_package("prospectr")

#' Read files from various formats into `spectra`
#'
#' @param path Path to directory or input files
#' @param format file formats. "asd" (for ASD); "sig" or "svc" (for SVC);
#'               "sed" or "psr" (for SpecEvo PSR).
#' @param include_white_ref boolean. include white reference. NOT IMPLEMENTED YET
#' @param recursive read files recursively
#' @param exclude_if_matches excludes files that match this regular expression.
#'                           Example: "BAD"
#' @param outside_01_action what to do with values outside 0 and 1? Options are
#'                          "NA", which replaces those values with NA or
#'                          "nothing" (default).
#' @param ... nothing yet
#' @return a single `spectra` or a list of `spectra` (in case files have
#'         incompatible band number or wavelengths values)
#'
#' @author Jose Eduardo Meireles
#' @export
read_spectra = function(path,
                        format,
                        include_white_ref  = FALSE,
                        recursive          = FALSE,
                        exclude_if_matches = NULL,
                        outside_01_action  = "nothing", # "NA"
                        ...) {

    #########################################
    ## match formats
    #########################################
    format_lookup = c(sig = "sig",
                      svc = "sig",
                      sed = "sed",
                      psr = "sed",
                      asd = "asd")
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
        # bad_tag = paste0("\\.", exclude_if_matches, "$")
        # m       = grepl(pattern = bad_tag, i_path)
        bad_tag = exclude_if_matches
        m       = grepl(paste(bad_tag,collapse = "|"), i_path)
        i_path  = i_path[!m]
    }

    ## error if bad spectra fiters out all file names
    if(length(i_path) == 0){
        stop("No paths left after removeing bad spectra. Check your `exclude_if_ends_with` param")
    }


    #########################################
    ## define behaviour refl outside 01
    #########################################

    if(outside_01_action == "nothing"){
        fix_out01 = function(x){ x }

    } else if (outside_01_action %in% c("NA", "na", NA) ) {
        fix_out01 = function(x){
            x[ x < 0.0 || x > 1.0] = NA
            x
        }
    } else {
        stop("outside_01_action must be either `nothing` or `NA`")
    }

    #############################################################
    ## Read spectra with the appropriate function
    #############################################################

    if(format_lookup[format_match] == "sig"){
        result = i_read_ascii_spectra(i_path,
                                      skip_first_n      = 25,
                                      sep_char          = "",
                                      header            = FALSE,
                                      wl_col            = 1,
                                      refl_cols         = 4,
                                      divide_refl_by    = 100,
                                      include_white_ref = include_white_ref,
                                      outside_01_fun    = fix_out01)
        return(result)
    }

    if(format_lookup[format_match] == "sed"){

        result = i_read_ascii_spectra(i_path,
                                      skip_first_n      = 26,
                                      sep_char          = "\t",
                                      header            = TRUE,
                                      wl_col            = "Wvl",
                                      refl_cols         = c("Reflect. %", "Reflect. [1.0]"),
                                      divide_refl_by    = c(100, 1),
                                      include_white_ref = include_white_ref,
                                      outside_01_fun    = fix_out01,
                                      check.names       = FALSE)
        return(result)
    }

    if(format_lookup[format_match] == "asd"){
        result = i_read_asd_spectra(i_path,
                                    format            = "binary",
                                    divide_refl_by    = 1,
                                    include_white_ref = FALSE,
                                    outside_01_fun    = NULL,
                                    ...)
        return(result)
    }
}


#' Internal parser for ASCII format
#'
#' Generic parser for SVC's `.sig` and PSR's `.sed`
#'
#' @param file_paths paths for files already parsed by `read_spectra`
#' @param skip_first_n skip the first n lines
#' @param sep_char separator
#' @param header boolean. keep header?
#' @param wl_col idx or name of wavelength column
#' @param refl_cols idx or name of reflectance columns. MULTIPLE
#' @param divide_refl_by divide reflectance values by this. MULTIPLE
#' @param include_white_ref NOT USED YET, but should read the write reference
#'                          from each file
#' @param outside_01_fun function to deal with reflectance values outside 0.1.
#' @param ... additional arguments passed to read table
#' @return single `spectra` or list of `spectra`
#'
#' @importFrom utils read.delim
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_read_ascii_spectra = function(file_paths,
                                skip_first_n,
                                sep_char,
                                header,
                                wl_col,
                                refl_cols,
                                divide_refl_by,
                                include_white_ref,
                                outside_01_fun,
                                ...){

    ############################################################
    ## Internal function to read table
    ############################################################
    parse = function(x) {
        result = utils::read.delim(x, sep = sep_char,
                                   skip = skip_first_n, header = header, ...)
    }

    ############################################################
    ## Requirements and param checks

    if(length(refl_cols) > 1 && any(i_is_whole(refl_cols)) ){
        stop("refl_cols cannot be a vector of indices.")
    }

    ## Deal with cases where multiple reflectance columns or multiple reflectance
    ## scalars (divide_refl_by) are given
    if(length(refl_cols) < length(divide_refl_by)) {
        warning("Length of divide_refl_by should be either 1 or equals to the length of refl_cols. divide_refl_by has been prunned to length", length(refl_cols), ".")
        divide_refl_by = rep(divide_refl_by, length.out = length(refl_cols))
    }

    ############################################################
    ## Parse data
    ############################################################
    data        = lapply(file_paths, parse)
    names(data) = file_paths

    ############################################################
    ## Choose right reflectance columns from parsed data
    ## Updates refl_cols to an INDEX
    if(length(refl_cols) > 1) {

        d = data[[1]]                                 ## OK to use the first file as representative of the whole thing?
        m = colnames(d) %in% refl_cols
        n = which(refl_cols %in% colnames(d))

        if(all( !m )){
            stop("refl_cols did not match any columns.")
        }

        if( sum(m) > 1 ){
            stop("refl_cols matched more than one column.")
        }

        # Update refl cols and divide by
        # subset 1st as a safeguard in case m matches more than one column
        refl_cols      = which(m)
        divide_refl_by = divide_refl_by[n]
    }


    ## there mabye files with different number of bands or wavelength values
    ## check for them and split the data if needed
    wl_factor = unlist(
        lapply(data, function(x){
            paste0(x[ , wl_col], collapse = "")
        })
    )

    data = unname(split(data, wl_factor))

    ## Construct spectra
    spec = lapply(data, function(x) {
        rf = lapply(x, function(y){ y[ , refl_cols ] })
        rf = do.call(rbind, rf)
        rf = rf / divide_refl_by
        rf = outside_01_fun(rf)
        wl = x[[1]][ , wl_col ]
        nm = basename(names(x))

        spectra(rf, wl, nm)
    })

    if(length(spec) > 1){
        warning("Returning a list of `spectra` beause some files had different number of bands or wavelength values. If you want to make those data compatible, consider resampling (with resample) and then combining them (with combine)")
        return(spec)
    } else {
        return(spec[[1]])
    }
}


#' Parser for ASD's `.asd`
#'
#' @param file_paths paths for files already parsed by `read_spectra`
#' @param format choice of ASD format. Either "binary" or "txt"
#' @param divide_refl_by divide reflectance values by this
#' @param include_white_ref NOT USED YET, but should read the write reference
#'                          from each file
#' @param outside_01_fun function to deal with reflectance values outside 0.1.
#' @param ... NOT USED YET
#' @return spectra object
#'
#' @importFrom prospectr readASD
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_read_asd_spectra = function(file_paths,
                              format = c("binary", "txt"),
                              divide_refl_by,
                              include_white_ref,
                              outside_01_fun,
                              ...){

    rf = prospectr::readASD(fnames = file_paths, out_format = "matrix")
    wl = colnames(rf)
    nm = gsub(".asd$", "",rownames(rf))
    spectra(rf, wl, nm)
}

