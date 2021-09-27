#' Title
#'
#' @param SelectedProtein
#' @param dfPeptidesColors
#' @param proteaseSelected
#'
#' @return
#' @export
#'
#' @examples
connectProtterAPI <- function(dfPeptidesColors = NULL,
                              SelectedProtein = NULL,
                              proteaseSelected = 'Tryps'){



    # Generate url for protter API

    url <- paste0("http://wlab.ethz.ch/protter/create?up=",
                  SelectedProtein,
                  "&tm=auto&mc=lightsalmon&lc=blue&tml=numcount&")


    for (ii in seq_len(nrow(dfPeptidesColors))) {

        url <- paste0(url,"bc:", dfPeptidesColors$Colour[ii], "=",
                      dfPeptidesColors$Sequence[ii], '&' )
    }

    if (!proteaseSelected == 'none') {

        url <- paste0(url, "cutAt=peptidecutter.", proteaseSelected,"&")
    }

    url <- paste0(url, "format=svg")

    message(paste0('The url is: \n', url))

    return(url)
}
