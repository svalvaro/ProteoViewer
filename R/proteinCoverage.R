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
                            sizeSegments,
                            darkMode = TRUE,
                            nameCondition = NULL,
                            intensityRange = NULL){

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

    dfPeptidesColors$Length <- nchar(dfPeptidesColors$Sequence)

    # Now Match the peptides and add the Start/Finish

    dfPeptidesColors$startPosition <- stringi::stri_locate(str = proteinSequence, regex = dfPeptidesColors$Sequence)[,1]

    dfPeptidesColors$endPosition <- stringi::stri_locate(str = proteinSequence, regex = dfPeptidesColors$Sequence)[,2]

    # Add the hash to the colours

    dfPeptidesColors$Colour <- paste0("#", dfPeptidesColors$Colour)


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
            #theme_bw()+
            #theme_dark()+
            coord_cartesian(xlim = c(0, lengthProteinSequence))+
            ylab("Log2 Intensity")+
            ylim(intensityRange)


    }else if(yaxis == "startPosition"){
        p <- ggplot(dfPeptidesColors , aes(text = Sequence))+
            geom_segment(aes(x = startPosition,
                             xend = endPosition,
                             y = startPositionDuplicate,
                             yend = endPositionDuplicate
            ),colour = dfPeptidesColors$Colour,size = sizeSegments)+
            #theme_bw()+
            coord_cartesian(xlim = c(0, lengthProteinSequence), ylim = c(0, lengthProteinSequence))+
            ylab("Start Position")

    }

    p <- p +
        xlab("Start Position")

    if(!is.null(nameCondition)){
        p <- p + ggtitle(paste0("Protein Coverage:  ", nameCondition))
    }else{
        p <- p + ggtitle(paste0("Protein Coverage of:  ", proteinId))
    }



    if (darkMode == TRUE) {
        p <- p + theme_bw()+
            theme(panel.background = element_rect(fill = "#333333", colour = NA,
                                                 color = 'black'),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank())
    }else{
        p <- p + theme_bw()+
            theme(plot.background = element_rect(color = 'black',size = 2))
    }


    plotly::ggplotly(p,
                     tooltip = c("startPosition","endPosition","Intensity","Sequence"))

    }


