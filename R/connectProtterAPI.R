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
                              modifiedPeptides = NULL,
                              SelectedProtein = NULL,
                              proteaseSelected = 'Tryps'){

    # Generate url for protter API


    url <- paste0("http://wlab.ethz.ch/protter/create?up=",
                SelectedProtein,
                "&tm=auto&mc=lightsalmon&lc=blue&tml=numcount&")


    # Change it to comma separated color for peptides with the same color,
    # It will reduce a bit the length of the total query and might aliviate the
    # issues with long requests.


    if (!is.null(dfPeptidesColors)>0) {

        for (ii in seq_len(nrow(dfPeptidesColors))) {

            url <- paste0(url,"bc:", dfPeptidesColors$Colour[ii], "=",
                          dfPeptidesColors$Sequence[ii], '&' )
        }

    }

    # If modified peptides are added

    if(!is.null(modifiedPeptides)){

        # For Oxidation (M):

        # The indexes of modified peptides are:

        OxidationIndexes <- which(modifiedPeptides$Modifications == 'Oxidation (M)')

        if (length(OxidationIndexes)>0) {

            # Add the first peptide with Oxidation (M)
            url <- paste0(url, 'modMox=', modifiedPeptides$Modified.sequence[OxidationIndexes[1]])

            # If there are more than one oxidations, add commas between the peptides
            # since the first one has been added
            if (length(OxidationIndexes)>1) {

                # [-1] since the first modified peptide has been added to the url already
                for (ii in OxidationIndexes[-1]) {

                    url <- paste0(url,',', modifiedPeptides$Modified.sequence[ii])
                }
            }

            url <- paste0(url, '&')
        }

        # For Acetyl (Protein N-term)

        # The indexes of modified peptides are:

        AcetylationIndexes <- which(modifiedPeptides$Modifications == 'Acetyl (Protein N-term)')

        if (length(AcetylationIndexes)>0) {

            # Add the first peptide with Oxidation (M)
            url <- paste0(url, 'modnac=', modifiedPeptides$Modified.sequence[AcetylationIndexes[1]])

            # If there are more than one oxidations, add commas between the peptides
            # since the first one has been added
            if (length(AcetylationIndexes)>1) {

                # [-1] since the first modified peptide has been added to the url already
                for (ii in AcetylationIndexes[-1]) {

                    url <- paste0(url,',', modifiedPeptides$Modified.sequence[ii])
                }
            }

            url <- paste0(url, '&')
        }




    }


    if (!proteaseSelected == 'none') {

        url <- paste0(url, "cutAt=peptidecutter.", proteaseSelected,"&")
    }

    url <- paste0(url, "format=svg")

    message(paste0('The url is: \n', url))

    return(url)
}
