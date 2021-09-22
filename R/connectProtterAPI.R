#' Title
#'
#' @param evidence
#' @param SelectedExperiment
#' @param SelectedProtein
#' @param combineExperiments
#'
#' @return
#' @export
#'
#' @examples
connectProtterAPI <- function(evidence = NULL,
                              SelectedExperiment = NULL,
                              SelectedProtein = NULL,
                              combineExperiments = FALSE){


    if (!is.data.frame(evidence)) {
        return(NULL)
    }

    print(nrow(evidence))
    # SelectedExperiment = "wt_1"
    # SelectedProtein = "O75947"

    proteomicsInput <- evidence %>%
        dplyr::select(
        c(
            dplyr::contains(c('Proteins',
                              'Experiment',
                              'Protein.names',
                              'Sequence',
                              'Intensity')),
            - dplyr::contains(c('leading',
                                'max',
                                'modified'))
        )
    )



    # Select only for the selected protein

    # ProteoIndexed <- df[df$Proteins == SelectedProtein,]
    ProteoIndexed <- proteomicsInput[
        proteomicsInput$Proteins == SelectedProtein,]

    if (combineExperiments == FALSE) {
        # ProteoIndexed <- df[df$Proteins == SelectedProtein,]
        ProteoIndexed <- ProteoIndexed[
            ProteoIndexed$Experiment == SelectedExperiment,]
    }


    # Remove rows containing NAs in the Intensity colum

    ProteoIndexed <- ProteoIndexed[!is.na(ProteoIndexed$Intensity),]



    colors = nrow(ProteoIndexed)

    ProteoIndexed$Colour <- grDevices::colorRampPalette(c("blue", "red"))(colors)

    ProteoIndexed$Colour <- base::gsub('#', '', ProteoIndexed$Colour)





    # Generate url for protter API

    url <- paste0("http://wlab.ethz.ch/protter/create?up=",
                  SelectedProtein,
                  "&tm=auto&mc=lightsalmon&lc=blue&tml=numcount&")


    for (ii in seq_len(nrow(ProteoIndexed))) {

        url <- paste0(url,"bc:", ProteoIndexed$Colour[ii], "=",
                      ProteoIndexed$Sequence[ii], '&' )
    }


    url <- paste0(url, "format=svg")


    #url <- paste0("http://wlab.ethz.ch/protter/create?up=",input$SelectedProtein,"&tm=auto&mc=lightsalmon&lc=blue&tml=numcount&bc:yellow=C&bc:green=I&format=svg")


    return(url)

}
