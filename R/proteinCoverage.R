#' Title
#'
#' @param proteinId
#' @param evidence
#'
#' @return
#' @export
#'
#'
#' @examples
proteinCoverage <- function(proteinId,
                            dfPeptidesColors,
                            yaxis = c('Intensity', 'startPosition'),
                            comparison = c('individualExperiments',
                                           'combineExperiments',
                                           'conditions')){

    uniprot_url <- "http://www.uniprot.org/uniprot/"

    uniprot_request <- paste0(uniprot_url, proteinId, '.fasta')

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


    # if (comparison == "conditions") {
    #
    #
    # }

    # Duplicate columns to solve the duplication in the tooltip with ggplotly

    dfPeptidesColors$Intensity <- format(round(dfPeptidesColors$Intensity , 2), nsmall = 2)

    dfPeptidesColors$startPositionDuplicate <- dfPeptidesColors$startPosition
    dfPeptidesColors$endPositionDuplicate <- dfPeptidesColors$endPosition
    dfPeptidesColors$IntensityDuplicate <- dfPeptidesColors$Intensity



    if (yaxis == "Intensity") {

        p <- ggplot(dfPeptidesColors, aes(text = Sequence))+
            geom_segment(aes(x = startPosition,
                             xend = endPosition,
                             y = as.numeric(Intensity),
                             yend = as.numeric(IntensityDuplicate)
            ),
            colour = dfPeptidesColors$Colour,
            size = 2)+
            theme_bw()+
            coord_cartesian(xlim = c(0, lengthProteinSequence))+
            ylab("Log2 Intensity")


    }else if(yaxis == "startPosition"){
        p <- ggplot(dfPeptidesColors , aes(text = Sequence))+
            geom_segment(aes(x = startPosition,
                             xend = endPosition,
                             y = startPositionDuplicate,
                             yend = endPositionDuplicate
            ),colour = dfPeptidesColors$Colour,size = 2)+
            theme_bw()+
            coord_cartesian(xlim = c(0, lengthProteinSequence), ylim = c(0, lengthProteinSequence))+
            ylab("Start Position")

    }

    p <- p +
        xlab("Start Position")+
        ggtitle("Coverage of the Protein")


    plotly::ggplotly(p,
                     tooltip = c("startPosition","endPosition","Intensity","Sequence"))

    }


