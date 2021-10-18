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

    modifiedPeptides <- evidence %>% dplyr::select(c(
        'Sequence', 'Modifications', 'Modified.sequence',
        'Proteins', 'Experiment', 'Intensity'
    ))


    modifiedPeptides <- modifiedPeptides[!is.na(modifiedPeptides$Intensity),]

    if(! is.null(selectedExperiment)){

        modifiedPeptides <- modifiedPeptides[
            modifiedPeptides$Experiment == selectedExperiment,]
    }

    # Index by Selected protein and remove Unmodified peptides

    # selectedProtein = 	'P10645'

    modifiedPeptides <- modifiedPeptides[
        modifiedPeptides$Proteins == selectedProtein,]
}
