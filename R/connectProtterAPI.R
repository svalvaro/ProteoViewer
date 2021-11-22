#' Title
#'
#' @param selectedProtein
#' @param dfPeptidesColors
#' @param proteaseSelected
#'
#' @return
#' @export
#'
#' @examples
connectProtterAPI <- function(dfPeptidesColors = NULL,
                              modifiedPeptides = NULL,
                              selectedProtein = NULL,
                              proteaseSelected = 'Tryps'){

    # Generate url for protter API

    # If the protein is a contaminant (CON__) it needs to be removed that part.

    selectedProtein <- gsub(pattern = 'CON__',
                            replacement = '',
                            x = selectedProtein)


    url <- paste0("http://wlab.ethz.ch/protter/create?up=",
                selectedProtein,
                "&tm=auto&mc=lightsalmon&lc=blue&tml=numcount&")


    # Change it to comma separated color for peptides with the same color,
    # It will reduce a bit the length of the total query and might aliviate the
    # issues with long requests.

    if (!is.null(dfPeptidesColors)>0) {

        colorsCollapsed <- dfPeptidesColors %>%
            group_by(Colour) %>%
            summarise(Seq = base::toString(Sequence)) %>%
            ungroup()

        colorsCollapsed$Seq <- gsub(' ','', colorsCollapsed$Seq)

        for (ii in seq_len(nrow(colorsCollapsed))) {

            url <- paste0(url,"bc:", colorsCollapsed$Colour[ii], "=",
                          colorsCollapsed$Seq[ii], '&' )
        }

        # for (ii in seq_len(nrow(dfPeptidesColors))) {
        #
        #     url <- paste0(url,"bc:", dfPeptidesColors$Colour[ii], "=",
        #                   dfPeptidesColors$Sequence[ii], '&' )
        # }

    }


    #### PTMS ####
    # If modified peptides are added, then a new part will be added to the url:
    # oxidation in methynines will be added in the form:
    # '...&modMox=' and the peptides separated by commas.

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


        # For Methyl (KR):

        # The indexes of modified peptides are:

        MetKRIndexes <- which(modifiedPeptides$Modifications == 'Methyl (KR)')

        if (length(MetKRIndexes)>0) {

            # Add the first peptide with Oxidation (M)
            url <- paste0(url, 'modMetKR=', modifiedPeptides$Modified.sequence[MetKRIndexes[1]])

            # If there are more than one oxidations, add commas between the peptides
            # since the first one has been added
            if (length(MetKRIndexes)>1) {

                # [-1] since the first modified peptide has been added to the url already
                for (ii in MetKRIndexes[-1]) {

                    url <- paste0(url,',', modifiedPeptides$Modified.sequence[ii])
                }
            }

            url <- paste0(url, '&')
        }



        # For Dimethyl (KR):

        # The indexes of modified peptides are:

        DiMetKRIndexes <- which(modifiedPeptides$Modifications == 'Dimethyl (KR)')

        if (length(DiMetKRIndexes)>0) {

            # Add the first peptide with Oxidation (M)
            url <- paste0(url, 'modDiMetKR=', modifiedPeptides$Modified.sequence[DiMetKRIndexes[1]])

            # If there are more than one oxidations, add commas between the peptides
            # since the first one has been added
            if (length(DiMetKRIndexes)>1) {

                # [-1] since the first modified peptide has been added to the url already
                for (ii in DiMetKRIndexes[-1]) {

                    url <- paste0(url,',', modifiedPeptides$Modified.sequence[ii])
                }
            }

            url <- paste0(url, '&')
        }



        # For Trimethyl (K):

        # The indexes of modified peptides are:

        TriMetKIndexes <- which(modifiedPeptides$Modifications == 'Trimethyl (K)')

        if (length(TriMetKIndexes)>0) {

            # Add the first peptide with Oxidation (M)
            url <- paste0(url, 'modTriMetK=', modifiedPeptides$Modified.sequence[TriMetKIndexes[1]])

            # If there are more than one oxidations, add commas between the peptides
            # since the first one has been added
            if (length(TriMetKIndexes)>1) {

                # [-1] since the first modified peptide has been added to the url already
                for (ii in TriMetKIndexes[-1]) {

                    url <- paste0(url,',', modifiedPeptides$Modified.sequence[ii])
                }
            }

            url <- paste0(url, '&')
        }

        # For Phospho (STY):

        # The indexes of modified peptides are:

        PhosphoSTYIndexes <- which(modifiedPeptides$Modifications == 'Phospho (STY)')

        if (length(PhosphoSTYIndexes)>0) {

            # Add the first peptide with Oxidation (M)
            url <- paste0(url, 'modPhosphoSTY=', modifiedPeptides$Modified.sequence[PhosphoSTYIndexes[1]])

            # If there are more than one oxidations, add commas between the peptides
            # since the first one has bemodPhosphoSTYen added
            if (length(PhosphoSTYIndexes)>1) {

                # [-1] since the PhosphoSTYIndexes modified peptide has been added to the url already
                for (ii in PhosphoSTYIndexes[-1]) {

                    url <- paste0(url,',', modifiedPeptides$Modified.sequence[ii])
                }
            }

            url <- paste0(url, '&')
        }


    }


    # This part creates the style of in the rendered protein of the PTMs
    # s: shape
    # bc" background color
    # cc: contour color


    if ('Oxidation (M)' %in% modifiedPeptides$Modifications) {
        url <- paste0(url, 's:diamond,bc:forestgreen,cc:white=EX.MODMOX&' )
    }

    if ('Acetyl (Protein N-term)' %in% modifiedPeptides$Modifications) {
        url <- paste0(url, 's:diamond,bc:yellow,cc:black=EX.MODnac&' )
    }

    if ('Methyl (KR)' %in% modifiedPeptides$Modifications) {
        url <- paste0(url, 's:diamond,bc:red,cc:white=EX.MODMetKR&' )
    }

    if ('Dimethyl (KR)' %in% modifiedPeptides$Modifications) {
        url <- paste0(url, 's:diamond,bc:blue,cc:white=EX.MODDiMetKR&' )
    }

    if ('Trimethyl (K)' %in% modifiedPeptides$Modifications) {
        url <- paste0(url, 's:diamond,bc:purple,cc:white=EX.MODTriMetK&' )
    }

    if ('Phospho (STY)' %in% modifiedPeptides$Modifications) {
        url <- paste0(url, 's:diamond,bc:purple,cc:white=EX.modPhosphoSTY&' )
    }


    if (!proteaseSelected == 'none') {

        url <- paste0(url, "cutAt=peptidecutter.", proteaseSelected,"&")
    }

    #url <- paste0(url, "format=png")
    url <- paste0(url, "format=svg")

    message(paste0('The url is: \n', url))

    return(url)
}
