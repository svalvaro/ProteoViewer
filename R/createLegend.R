#' Title
#'
#' @param evidence
#' @param SelectedExperiment
#' @param selectedProtein
#' @param combineExperiments
#' @param plot_legend
#' @param experimentDesign
#' @param conditionSelected
#'
#' @return
#' @export
#'
#' @examples
createLegend <- function(evidence,
                        peptideType = c("Unmodified", "Modified", "Both"),
                        experimentDesign,
                        comparison = c('individualExperiments',
                                       'combineExperiments',
                                       'conditions'),
                        conditionSelected = NULL,
                        selectedExperiment = NULL,
                        selectedProtein = NULL,
                        #combineExperiments = TRUE,
                        plot_legend = TRUE){

    #### First part ####

    if (peptideType == 'Unmodified') {
        evidence <- evidence[evidence$Modifications == 'Unmodified',]
    }

    if (peptideType == 'Modified') {
        evidence <- evidence[! evidence$Modifications == 'Unmodified',]
    }

    # If the peptide type is "Both" do nothing.

    # Creation of a plot containing a table with and a plot with the intensities
    # coloured.

    if (!is.data.frame(evidence)) {
        return(NULL)
    }

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

    # If there are no peptides are found in that experiment.

    if (nrow(proteomicsInput) == 0) {
        message('No peptides found in this experiment for this protein.')
        return(NULL)
    }

    # Remove rows containing NAs in the Intensity colum

    proteomicsInput <- proteomicsInput[!is.na(proteomicsInput$Intensity),]

    # First aggregate the sum of the same peptide for the same experiment,
    # Because this step has to be done for the three possible conditions:

    dfPeptidesColors <- proteomicsInput %>%
        group_by(Sequence, Experiment, Proteins) %>%
        summarise(Intensity = sum(Intensity))

    # For the palette and for obtaining matching the color to the intensities:
    # I need to group the sequence independently of the experiment:

    dfColorsMax <- proteomicsInput %>%
        group_by(Sequence, Proteins) %>%
        summarise(Intensity = sum(Intensity))

    # The maximum colors will be combining all peptides by the sum

    dfColorsMax$Intensity <- log2(dfColorsMax$Intensity)

    dfColorsMax <- dfColorsMax[base::order(dfColorsMax$Intensity), ]

    dfColorsMax$Intensity <- as.numeric(dfColorsMax$Intensity)
    # Remove NAs

    dfColorsMax <- dfColorsMax[!is.na(dfColorsMax$Intensity),]

    # Minimium colours

    dfColorsMin <- dfPeptidesColors

    # The minimum colors will be those one when aggregated by experiment by sum

    dfColorsMin$Intensity <- log2(dfColorsMin$Intensity)

    dfColorsMin <- dfColorsMin[base::order(dfColorsMin$Intensity), ]

    dfColorsMin$Intensity <- as.numeric(dfColorsMin$Intensity)

    dfColorsMin <- dfColorsMin[!is.na(dfColorsMin$Intensity),]

    # Obtain the number of breaks to

    continuous_breaks <- seq(from = trunc(min(dfColorsMin$Intensity)),
                             to = trunc(max(dfColorsMax$Intensity))+1)

    myColors <- grDevices::colorRampPalette(c(
        "#2166AC",
        "#D1E5F0",
        '#ECF5BC',
        "#FDDBC7",
        "#B2182B"
    ))

    colorsToPlot <- myColors(length(continuous_breaks))

    # data frame to be plotted to generate the legend.
    dfToPlot <- data.frame(breaks = continuous_breaks,
                           Colour = colorsToPlot)

    # Plot the palette if required

    if (plot_legend == TRUE) {

        p <- ggplot(dfToPlot, aes(x = as.factor(breaks),
                                  y = 1,
                                  fill = as.factor(breaks)))+
            geom_bar(width = 1, stat = 'identity')+
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

    }

    ##### Second part #####

    # If the legend is not required, then we can now index by the protein

    dfPeptidesColors <- dfPeptidesColors[
        dfPeptidesColors$Proteins == selectedProtein,]

    # If there are no peptides are found in that experiment.

    if (nrow(dfPeptidesColors) == 0) {
        message('No peptides found in this experiment for this protein.')
        return(NULL)
    }

    # Remove rows containing NAs in the Intensity colum

    dfPeptidesColors <- dfPeptidesColors[!is.na(dfPeptidesColors$Intensity),]

    # First aggregate the sum of the same peptide for the same experiment,
    # Because this step has to be done for the three possible conditions:

    # If the plotting of the palette is not requried, I need to match
    # the peptide intensites to the color

    if (comparison == 'individualExperiments') {
        # Index by the experiment
        dfPeptidesColors <- dfPeptidesColors[
            dfPeptidesColors$Experiment == selectedExperiment,]
    }

    if (comparison == 'combineExperiments') {

        dfPeptidesColors <-  dfPeptidesColors %>%
            group_by(Sequence) %>%
            summarise(Intensity = sum(Intensity))
    }

    if (comparison == 'conditions') {

        # Match the experiments to the conditions
        dfPeptidesColors$Condition <- experimentDesign$condition[
            match(dfPeptidesColors$Experiment,experimentDesign$label)]

        # Index by the conditionSelected

        dfPeptidesColors <- dfPeptidesColors[
            dfPeptidesColors$Condition == conditionSelected,]

        # Obtain the median of the condition

        dfPeptidesColors <- dfPeptidesColors %>%
            group_by(Sequence) %>%
            summarise(Intensity = median(Intensity))
    }

    # Remove rows containing NAs in the Intensity colum

    dfPeptidesColors <- dfPeptidesColors[!is.na(dfPeptidesColors$Intensity),]

    # Calculate the log2 of the intensities and reorder

    dfPeptidesColors$Intensity <- log2(dfPeptidesColors$Intensity)

    dfPeptidesColors <- dfPeptidesColors[
        base::order(dfPeptidesColors$Intensity),]

    dfPeptidesColors$Intensity <- as.numeric(dfPeptidesColors$Intensity)


    #   # Match the colour of the legend to each sequence to generate the image

    dfPeptidesColors$Colour <- dfToPlot$Colour[
        match( trunc(dfPeptidesColors$Intensity),dfToPlot$breaks)]

    # Remove the hash from the names, the Protter API doesn't accept them

    dfPeptidesColors$Colour <- base::gsub('#', '', dfPeptidesColors$Colour)

    if (nrow(dfPeptidesColors) == 0 && plot_legend == FALSE) {
        message('No peptides found in this experiment for this protein and
                experiment.')
        return(NULL)
    }

    return(dfPeptidesColors)
}
