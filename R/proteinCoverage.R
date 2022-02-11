#' Title
#'
#' @param proteinId
#' @param evidence
#'
#' @return
#' @export
#'
#'
#' @importFrom stringi str_locate
#'
#' @examples
proteinCoverage <- function(proteinId,
                            dfPeptidesColors,
                            comparison = c('individualExperiments',
                                           'combineExperiments',
                                           'conditions')){

    uniprot_url <- "http://www.uniprot.org/uniprot/"

    uniprot_request <- paste0(uniprot_url, proteinID, '.fasta')

    proteinSequence <- httr::GET(uniprot_request)

    proteinSequence   <- httr::content(proteinSequence, encoding = "UTF-8")

    lengthProteinSequence <- nchar(proteinSequence)

    # Keep only the sequence
    proteinSequence <- gsub(x = proteinSequence,
                             replacement = '',
                             pattern =  '.*.SV=.\n',
                             ignore.case = T)

    # Remove the \n
    proteinSequence <- gsub(x = proteinSequence,
         replacement = '',
         pattern = '\n',
         ignore.case = T)


    # proteomicsInput <- evidence %>%
    #     dplyr::select(
    #         c(
    #             dplyr::contains(c('Proteins',
    #                               'Experiment',
    #                               'Protein.names',
    #                               'Sequence',
    #                               'Intensity')),
    #             - dplyr::contains(c('leading',
    #                                 'max',
    #                                 'modified'))
    #         )
    #     )
    #
    # # Filter by the selected Protein
    # proteomicsInput <- proteomicsInput[proteomicsInput$Proteins == proteinID,]



    dfPeptidesColors$Length <- nchar(dfPeptidesColors$Sequence)

    # Now Match the peptides and add the Start/Finish

    dfPeptidesColors$startPosition <- stringi::stri_locate(str = proteinSequence, regex = dfPeptidesColors$Sequence)[,1]

    dfPeptidesColors$endPosition <- stringi::stri_locate(str = proteinSequence, regex = dfPeptidesColors$Sequence)[,2]

    # Add the hash to the colours

    dfPeptidesColors$Colour <- paste0("#", dfPeptidesColors$Colour)


    # Now filter by the comparison


    if (comparison == "conditions") {

    }




    p <- ggplot(dfPeptidesColors)+
        geom_segment(aes(x = startPosition,
                        xend = endPosition,
                        y = startPosition,
                        yend = endPosition
                        ),
                     colour = dfPeptidesColors$Colour,
                     size = 2)+
        theme_bw()+
        coord_cartesian(xlim = c(0, lengthProteinSequence), ylim = c(0, lengthProteinSequence))#+
        # scale_color_manual(values = dfPeptidesColors$Colour)

    plotly::ggplotly(p)

    }


