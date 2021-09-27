#' Title
#'
#' @param evidence
#' @param SelectedExperiment
#' @param SelectedProtein
#' @param combineExperiments
#' @param plot_legend
#'
#' @return
#' @export
#'
#' @examples
createLegend <- function(evidence = NULL,
                      SelectedExperiment = NULL,
                      SelectedProtein = NULL,
                      combineExperiments = FALSE,
                      plot_legend = TRUE){


    if (!is.data.frame(evidence)) {
        return(NULL)
    }

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


    # If there are no peptides are foun:d in that experiment.

    if (nrow(ProteoIndexed) == 0) {
        message('No peptides found in this experiment for this protein.')
        return(NULL)
    }

    # Remove rows containing NAs in the Intensity colum

    ProteoIndexed <- ProteoIndexed[!is.na(ProteoIndexed$Intensity),]

    ## Deal with repeated Sequences, obtain the average for now

    dfPeptidesColors <- ProteoIndexed %>%
        group_by(Sequence, Experiment) %>%
        summarise(Intensity = sum(Intensity))


    # Calculate the log2 of the intensities and reorder

    dfPeptidesColors$Intensity <- log2(dfPeptidesColors$Intensity)

    dfPeptidesColors <- dfPeptidesColors[base::order(dfPeptidesColors$Intensity), ]

    dfPeptidesColors$Intensity <- as.numeric(dfPeptidesColors$Intensity)


    # Create continous scale for the combined protein independtly of the
    # experiment. That way it is easy to compare the experiments against each
    # other


    continuous_breaks <- seq(from = trunc(min(dfPeptidesColors$Intensity)),
                             to = trunc(max(dfPeptidesColors$Intensity)))


    myColors <- grDevices::colorRampPalette(c(
        "#2166AC",
        "#D1E5F0",
        '#D0F5BC',
        "#FDDBC7",
        "#B2182B"
    ))

    colorsToPlot <- myColors(length(continuous_breaks))

    # data frame to be plotted to generate the legend.
    dfToPlot <- data.frame(breaks = continuous_breaks,
                           Colour = colorsToPlot)


    # Match the colour of the legend to each sequence to generate the image

    dfPeptidesColors$Colour <- dfToPlot$Colour[match( trunc(dfPeptidesColors$Intensity),dfToPlot$breaks)]

    # Remove the hash from the names, the Protter API doesn't accept them

    dfPeptidesColors$Colour <- base::gsub('#', '', dfPeptidesColors$Colour)


    # If the user now wants to select a specific experiment


    if (combineExperiments == FALSE) {
        # ProteoIndexed <- df[df$Proteins == SelectedProtein,]
        dfPeptidesColors <- dfPeptidesColors[
            dfPeptidesColors$Experiment == SelectedExperiment,]
    }



    if (nrow(dfPeptidesColors) == 0 &
        plot_legend == FALSE) {
        message('No peptides found in this experiment for this protein and
                experiment.')
        return(NULL)
    }

    # Plot the palette if required

    if (plot_legend == TRUE) {

        p <- ggplot(dfToPlot, aes(x = as.factor(breaks),
                                  y = 1,
                                  fill = as.factor(breaks)))+
            geom_bar(width = 1,
                     stat = 'identity'
            )+
            scale_fill_manual(values = dfToPlot$Colour)+
            ggtitle(base::expression('Log'[2]*' Intensity'))+
            ylab('')+
            theme_bw()+
            xlab('')+
            theme(legend.position = 'none',
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())+
            ylim(0,1)

        return(p)

    }else{
        return(dfPeptidesColors)
    }

}
