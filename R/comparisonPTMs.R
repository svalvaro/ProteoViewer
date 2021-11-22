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
                           peptideType = c("Unmodified", "Modified", "Both"),
                           selectedProtein,
                           selectedExperiment,
                           experimentDesign = NULL,
                           selectedConditions = NULL){

    if (peptideType == 'Unmodified') {
        evidence <- evidence[evidence$Modifications == 'Unmodified',]
    }

    if (peptideType == 'Modified') {
        evidence <- evidence[! evidence$Modifications == 'Unmodified',]
    }

    # Obtain the table
    modifiedPeptides <- evidence %>% dplyr::select(c(
        'Sequence', 'Modifications', 'Modified.sequence',
        'Proteins', 'Experiment', 'Intensity'
    ))


    # Index by selected protein
    modifiedPeptides <- modifiedPeptides[
        modifiedPeptides$Proteins == selectedProtein,]

    # modifiedPeptides <- modifiedPeptides[
    #     grep(pattern = selectedProtein,
    #          x = modifiedPeptides$Proteins),
    #     ]


    # Remove empty values
    modifiedPeptides <- modifiedPeptides[!is.na(modifiedPeptides$Intensity),]

    # Group by Sequence and modification, sum intensities
    modifiedPeptides <- modifiedPeptides %>%
        group_by(Sequence,  Modifications, Experiment) %>%
        summarise(Intensity = sum(Intensity))


    # Option 1: the user wants to see the only one experiment

    # If provide an experiment, index by it
    if(! is.null(selectedExperiment)){

        modifiedPeptides <- modifiedPeptides[
            modifiedPeptides$Experiment == selectedExperiment,]


        modifiedPeptides$Experiment <- NULL

    }


    # Option 2: The user wants to see the combined version of all experiments:

    # If provide an experiment, index by it
    if(is.null(selectedExperiment) && is.null(selectedConditions)){

        # Group by Sequence and modification, sum intensities
        modifiedPeptides <- modifiedPeptides %>%
            group_by(Sequence,  Modifications) %>%
            summarise(Intensity = sum(Intensity))
    }


    # Option 3: The user wants to compare one or two conditions:





    # If two conditions are compared, return a table with:
    # 'Condition' 'Sequence' 'Modification' and 'Intensity'
    if(!is.null(experimentDesign) && !is.null(selectedConditions)){

        # Match the conditions to the experiment design

        modifiedPeptides$Condition <- experimentDesign$condition[
            base::match(modifiedPeptides$Experiment, experimentDesign$label)
        ]

        # Select those conditions selected

        modifiedPeptides <- modifiedPeptides[
            modifiedPeptides$Condition %in% selectedConditions,]


        # Intensities is the SUM of the sequence and group, then the median of
        # the conditions

        # Group by Sequence and modification, sum intensities
        modifiedPeptides <- modifiedPeptides %>%
            group_by(Sequence,  Modifications, Condition) %>%
            summarise(Intensity = median(Intensity))

    }


    # Format the Intensity column to log2 and only one decimal
    modifiedPeptides$Intensity <- format(
        round(
            log2(modifiedPeptides$Intensity),
            digits = 1
        ), nsmall = 1
    )

    return(modifiedPeptides)

}
