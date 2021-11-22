#' Title
#'
#' @param evidence
#' @param selectedProtein
#' @param selectedExperiment
#' @param experimentDesign
#' @param selectedCondition
#' @param plotLegend
#'
#' @importFrom grid rasterGrob
#' @importFrom  gridExtra grid.arrange
#' @return
#' @export
#'
#' @examples
createPTMs <- function(evidence,
                      peptideType = c("Unmodified", "Modified", "Both"),
                      selectedProtein,
                      selectedExperiment,
                      experimentDesign = NULL,
                      selectedCondition = NULL,
                      plotLegend = TRUE){

  if (peptideType == 'Unmodified') {
    evidence <- evidence[evidence$Modifications == 'Unmodified',]
  }

  if (peptideType == 'Modified') {
    evidence <- evidence[! evidence$Modifications == 'Unmodified',]
  }

    modifiedPeptides <- evidence %>% dplyr::select(c(
      'Sequence', 'Modifications', 'Modified.sequence',
      'Proteins', 'Experiment'
  ))

  if(! is.null(selectedExperiment)){

    modifiedPeptides <- modifiedPeptides[
      modifiedPeptides$Experiment == selectedExperiment,]
  }

  # Index by Selected protein and remove Unmodified peptides

  # selectedProtein = 	'P10645'

  # modifiedPeptides <- modifiedPeptides[
  #   grep(pattern = selectedProtein,
  #        x = modifiedPeptides$Proteins),
  #   ]

  modifiedPeptides <- modifiedPeptides[
    modifiedPeptides$Proteins == selectedProtein,]

  modifiedPeptides <- modifiedPeptides[
    ! modifiedPeptides$Modifications == 'Unmodified',]

  modifiedPeptides <- base::unique(modifiedPeptides)

  # If Experiment Design is provided (when comparing conditions)

  if(!is.null(experimentDesign) && !is.null(selectedCondition)){
    modifiedPeptides$Condition <- experimentDesign$condition[
      base::match(modifiedPeptides$Experiment, experimentDesign$label)
    ]

    modifiedPeptides <- modifiedPeptides[
      modifiedPeptides$Condition == selectedCondition,]
  }

  # In order to colour individually the modifications, only one modification
  # can exist per row. For those peptides containing multiple modificaitons:

  modifiedPeptides <-  modifiedPeptides %>%
    tidyr::separate_rows(Modifications,
                         sep =  ',\\s?')

  # Remove the 1 , 2 , 3  and PTM since it's not needed.
  modifiedPeptides$Modifications <-   gsub(
    pattern = '[1-3] ',
    replacement =  '',
    x =  modifiedPeptides$Modifications)


  for (ii in seq_len(nrow(modifiedPeptides))) {

    # Obtain the total modifications and add parenthesis: '(Oxidation (M))'
    Modifications <- base::paste0('(',
                                  base::unique(
                                    modifiedPeptides$Modifications),
                                  ')')

    # Modifications to remove, is the list of modifications withouth the
    # corresponding modification of that sequence

    modsToRemove <- Modifications[! Modifications %in%
                                paste0('(',
                                       modifiedPeptides$Modifications[ii],
                                       ')')]

    # Add back slash to the parentheses
    modsToRemove <- gsub(pattern = '\\(', replacement = '\\\\(', modsToRemove)

    modsToRemove <- gsub(pattern = '\\)', replacement = '\\\\)', modsToRemove)

    # Collapse the modifications to remove together to make it regex pattern

    modsToRemove <- paste(unlist(modsToRemove), collapse = '|')

    # Remove all the modifications for the modified sequence (except the one)
    # that is in the modifiedPeptides$Modification

    modifiedPeptides$Modified.sequence[ii] <- gsub(
      pattern = modsToRemove,
      replacement = '',
      x = modifiedPeptides$Modified.sequence[ii] )
  }

  # Newer version of MaxQant

  # For rare cases where there is an (Acetyl Protein N-term ) &
  # (M Oxidations) in the same M:

  modifiedPeptides$Modified.sequence <-
      gsub(
          pattern = '\\(Acetyl \\(Protein N-term\\)\\)M\\(Oxidation \\(M\\)\\)',
          replacement = '(M)',
          modifiedPeptides$Modified.sequence
      )

  # For (M Oxidations):

    modifiedPeptides$Modified.sequence <-
      gsub(
      pattern = 'M\\(Oxidation \\(M\\)\\)',
      replacement = '(M)',
      modifiedPeptides$Modified.sequence
  )

  # For (Acetyl Protein N-term ) # Put the next one in parenthesis.

  modifiedPeptides$Modified.sequence <-
      gsub(pattern = '\\(Acetyl \\(Protein N-term\\)\\)(.)',
           replacement = '(\\1)',
           x = modifiedPeptides$Modified.sequence
  )

  # For Methyl (KR)

  modifiedPeptides$Modified.sequence <-
      gsub(pattern = '(K|R)\\(Methyl \\(KR\\)\\)',
           replacement = '(\\1)',
           x = modifiedPeptides$Modified.sequence
  )

  # For Dimethyl (KR)

  modifiedPeptides$Modified.sequence <-
      gsub(pattern = '(K|R)\\(Dimethyl \\(KR\\)\\)',
           replacement = '(\\1)',
           x = modifiedPeptides$Modified.sequence
  )

  # For Trimethyl (K)

  modifiedPeptides$Modified.sequence <-
      gsub(pattern = '(K)\\(Trimethyl \\(K\\)\\)',
           replacement = '(\\1)',
           x = modifiedPeptides$Modified.sequence
  )

  # For Phosphorylations (STY)

  modifiedPeptides$Modified.sequence <-
    gsub(pattern = '(S|T|Y)\\(Phospho \\(STY\\)\\)',
         replacement = '(\\1)',
         x = modifiedPeptides$Modified.sequence
    )



  # Remove the underscores:

  modifiedPeptides$Modified.sequence <-
      gsub(pattern = '_',
          replacement = '',
          x = modifiedPeptides$Modified.sequence)


  modifiedPeptides <-  unique(modifiedPeptides)

  if (plotLegend == FALSE) {
   return(modifiedPeptides)
  }

 # If the the legend has to be creaetd, the software has to select
 # the corresponding images from /www/ folder

  ptmsToPlot <- unique(modifiedPeptides$Modifications)

  if (length(ptmsToPlot)== 0) {
    message('No PTMs to create a legend')
    return(NULL)
  }

  ptmImages <- list()

  if ('Oxidation (M)' %in% ptmsToPlot) {

    p <- png::readPNG(
      source = system.file('shinyApp/www/legendImages/oxidationM.png',
                           package = 'ProteoViewer'))

    ptmImages[[length(ptmImages)+1]] <- p
  }

  if ('Acetyl (Protein N-term)' %in% ptmsToPlot) {

    p <- png::readPNG(
      source = system.file('shinyApp/www/legendImages/acetyl.png',
                           package = 'ProteoViewer'))

    ptmImages[[length(ptmImages)+1]] <- p
  }

  if ('Methyl (KR)' %in% ptmsToPlot) {

    p <- png::readPNG(
      source = system.file('shinyApp/www/legendImages/methyl.png',
                           package = 'ProteoViewer'))

    ptmImages[[length(ptmImages)+1]] <- p
  }

  if ('Dimethyl (KR)' %in% ptmsToPlot) {

    p <- png::readPNG(
      source = system.file('shinyApp/www/legendImages/Dimethyl.png',
                           package = 'ProteoViewer'))

    ptmImages[[length(ptmImages)+1]] <- p
  }

  if ('Trimethyl (K)' %in% ptmsToPlot) {

    p <- png::readPNG(
      source = system.file('shinyApp/www/legendImages/Trimethyl.png',
                           package = 'ProteoViewer'))

    ptmImages[[length(ptmImages)+1]] <- p
  }


  if ('Phospho (STY)' %in% ptmsToPlot) {

    p <- png::readPNG(
      source = system.file('shinyApp/www/legendImages/phospho.png',
                           package = 'ProteoViewer'))

    ptmImages[[length(ptmImages)+1]] <- p
  }

  ptmImages <- base::lapply(ptmImages, grid::rasterGrob)

  return(
    gridExtra::grid.arrange(grobs=ptmImages, ncol = 1)
  )
}
