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
                            xAxis = c('position', 'sequence'),
                            yaxis = c('Intensity', 'startPosition'),
                            sizeSegments,
                            backGroundColour = "#333333",
                            nameCondition = NULL,
                            intensityRange = NULL){

    uniprot_url <- "http://www.uniprot.org/uniprot/"

    uniprot_request <- paste0(uniprot_url, proteinId, '.fasta')

    proteinSequence <- httr::GET(uniprot_request)

    proteinSequence   <- httr::content(proteinSequence, encoding = "UTF-8")

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

    lengthProteinSequence <- nchar(proteinSequence)

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
            coord_cartesian(xlim = c(1, lengthProteinSequence))+
            ylab("Log2 Intensity")+
            ylim(intensityRange)

    }else if(yaxis == "startPosition"){
        p <- ggplot(dfPeptidesColors , aes(text = Sequence))+
            geom_segment(aes(x = startPosition,
                             xend = endPosition,
                             y = startPositionDuplicate,
                             yend = endPositionDuplicate
            ),colour = dfPeptidesColors$Colour,size = sizeSegments)+
            coord_cartesian(xlim = c(1, lengthProteinSequence), ylim = c(1, lengthProteinSequence))+
            ylab("Start Position")

    }else if (yaxis == 'sequence'){

        p <- ggplot(dfPeptidesColors , aes(text = Sequence))+
            geom_segment(aes(x = startPosition,
                             xend = endPosition,
                             y = startPositionDuplicate,
                             yend = endPositionDuplicate
            ),colour = dfPeptidesColors$Colour,size = sizeSegments)+
            coord_cartesian(xlim = c(1, lengthProteinSequence), ylim = c(1, lengthProteinSequence))+
            ylab("Sequence")

        yAxisSeq <- unlist(strsplit(proteinSequence, split = "+"))

        p <- p + scale_y_continuous(labels=yAxisSeq, breaks=1:length(yAxisSeq), limits=c(1,length(yAxisSeq)))

    }

    # For the X- axis

    if (xAxis == 'sequence') {
        xAxisSeq <- unlist(strsplit(proteinSequence, split = "+"))

        p <- p + scale_x_continuous(labels=xAxisSeq, breaks=1:length(xAxisSeq),
                                    limits=c(1,length(xAxisSeq)))+
            xlab('Start Position')
    }else {
        p <- p + xlab('Start Position')
    }

    if(!is.null(nameCondition)){
        p <- p + ggtitle(paste0("Protein Coverage:  ", nameCondition))
    }else{
        p <- p + ggtitle(paste0("Protein Coverage of:  ", proteinId))
    }

    p <- p + theme_bw()+
        theme(panel.background = element_rect(fill = backGroundColour, colour = NA,
                                             color = 'black'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank())

     plotly::ggplotly(p,
                     tooltip = c("startPosition","endPosition","Intensity","Sequence"))

    }


