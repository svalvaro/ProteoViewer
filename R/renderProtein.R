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

    utils::download.file(url = url, destfile = destfile)

    return(destfile)
}
