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

    # Remove rows containing NAs in the Intensity colum

    ProteoIndexed <- ProteoIndexed[!is.na(ProteoIndexed$Intensity),]

    ## Deal with repeated Sequences, obtain the average for now

    df <- ProteoIndexed %>%
        group_by(Sequence) %>%
        summarise(Intensity = sum(Intensity))


    # Calculate the log2 of the intensities and reorder

    df$Intensity <- log2(df$Intensity)

    df <- df[base::order(df$Intensity), ]

    # Only one decimal

    df$Intensity <- base::format(round(df$Intensity,3), nsmall = 3)


    # Assign the colors

    myColors <- grDevices::colorRampPalette(c(
        "#2166AC",
        "#D1E5F0",
        "#FDDBC7",
        "#B2182B"
    ))

    df$Colour <- myColors(nrow(df))

    #df$plotting_length <- 1


    # Plot the palette if required

    if (plot_palette == TRUE) {

        p <- ggplot(df, aes(x = as.factor(Intensity),
                            y = 1 ,
                            fill = as.factor(Intensity)))+
            geom_bar(width = 1,
                     stat = 'identity'
                     )+
            scale_fill_manual(values = df$Colour)+
            ggtitle(base::expression('Log'[2]*' Intensity'))+
            ylab('')+
            theme_bw()+
            xlab('')+
            theme(legend.position = 'none',
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()
                  )+
            ylim(0,1)

        return(p)

    }

    # Remove the hash from the names, the Protter API doesn't accept them

    df$Colour <- base::gsub('#', '', df$Colour)


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
