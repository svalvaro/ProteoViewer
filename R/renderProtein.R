#' Title
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
renderProtein <- function(url){

    # First download the picture as a temporary file
    if (is.null(url)) {
        return(NULL)
    }

    destfile <- base::tempfile(fileext = '.svg')

    message(paste0('The temporary directory for the file is: ', destfile))

    utils::download.file(url = url, destfile = destfile)

    return(destfile)
}
