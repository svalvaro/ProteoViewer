#' Title
#'
#' @param evidence
#' @param SelectedExperiment
#' @param SelectedProtein
#'
#' @return
#' @export
#'
#' @examples
connectProtterAPI <- function(evidence = NULL,
                              SelectedExperiment = NULL,
                              SelectedProtein = NULL,
                              combineExperiments = FALSE,
                              plot_palette = FALSE,
                              peptideCutter = TRUE){


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

    if (combineExperiments == FALSE) {
        # ProteoIndexed <- df[df$Proteins == SelectedProtein,]
        ProteoIndexed <- ProteoIndexed[
            ProteoIndexed$Experiment == SelectedExperiment,]
    }

    # If there are no peptides are found in that experiment.

    if (nrow(ProteoIndexed) == 0) {
        message('No peptides found in this experiment for this protein.')
        return(NULL)
    }

    # Remove rows containing NAs in the Intensity colum

    ProteoIndexed <- ProteoIndexed[!is.na(ProteoIndexed$Intensity),]

    ## Deal with repeated Sequences, obtain the average for now

    df <- ProteoIndexed %>%
        group_by(Sequence) %>%
        summarise(Intensity = sum(Intensity))


    # Calculate the log2 of the intensities and reorder

    df$Intensity <- log2(df$Intensity)

    df <- df[base::order(df$Intensity), ]

    df$Intensity <- as.numeric(df$Intensity)


    # Create continous scale


    continuous_breaks <- seq(from = trunc(min(df$Intensity)),
                             to = trunc(max(df$Intensity)))


    myColors <- grDevices::colorRampPalette(c(
        "#2166AC",
        "#D1E5F0",
        "#FDDBC7",
        "#B2182B"
    ))

    colorsToPlot <- myColors(length(continuous_breaks))

    # data frame to be plotted to generate the legend.
    dfToPlot <- data.frame(breaks = continuous_breaks,
                         Colour = colorsToPlot)


    # Match the colour of the legend to each sequence to generate the image

    df$Colour <- dfToPlot$Colour[match( trunc(df$Intensity),dfToPlot$breaks)]

    # Remove the hash from the names, the Protter API doesn't accept them

    df$Colour <- base::gsub('#', '', df$Colour)


    # Plot the palette if required

    if (plot_palette == TRUE) {

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

    }

    # Generate url for protter API

    url <- paste0("http://wlab.ethz.ch/protter/create?up=",
                  SelectedProtein,
                  "&tm=auto&mc=lightsalmon&lc=blue&tml=numcount&")


    for (ii in seq_len(nrow(df))) {

        url <- paste0(url,"bc:", df$Colour[ii], "=",
                      df$Sequence[ii], '&' )
    }

    if (peptideCutter == TRUE) {

        url <- paste0(url, "cutAt=peptidecutter.Tryps&")
    }

    url <- paste0(url, "format=svg")

    message(paste0('The url is: \n', url))

    return(url)
}
