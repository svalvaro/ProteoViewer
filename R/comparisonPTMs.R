#' Title
#'
#' @param evidence
#' @param selectedProtein
#' @param selectedExperiment
#' @param experimentDesign
#' @param selectedCondition
#'
#' @return
#' @export
#'
#' @examples
comparisonPTMs <- function(evidence,
                           selectedProtein,
                           selectedExperiment,
                           experimentDesign = NULL,
                           selectedCondition = NULL){

    # Obtain the table
    modifiedPeptides <- evidence %>% dplyr::select(c(
        'Sequence', 'Modifications', 'Modified.sequence',
        'Proteins', 'Experiment', 'Intensity'
    ))


    # Index by selected protein
    modifiedPeptides <- modifiedPeptides[
        modifiedPeptides$Proteins == selectedProtein,]


    # Remove empty values
    modifiedPeptides <- modifiedPeptides[!is.na(modifiedPeptides$Intensity),]

    # If provide an experiment, index by it
    if(! is.null(selectedExperiment)){

        modifiedPeptides <- modifiedPeptides[
            modifiedPeptides$Experiment == selectedExperiment,]

    }

    # Group by Sequence and modification, sum intensities
    modifiedPeptides <- modifiedPeptides %>%
        group_by(Sequence,  Modifications) %>%
        summarise(Intensity = sum(Intensity))

    # Format the Intensity column to log2 and only one decimal
    modifiedPeptides$Intensity <- format(
        round(
            log2(modifiedPeptides$Intensity),
            digits = 1
        ), nsmall = 1
    )

    return(modifiedPeptides)

}
