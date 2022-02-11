#' Title
#'
#' @param proteinId
#' @param evidence
#'
#' @return
#' @export
#'
#'
#' @importFrom stringi str_locate
#'
#' @examples
proteinCoverage <- function(proteinId, evidence){

    uniprot_url <- "http://www.uniprot.org/uniprot/"

    uniprot_request <- paste0(uniprot_url, proteinID, '.fasta')

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

    # Filter by the selected Protein
    proteomicsInput <- proteomicsInput[proteomicsInput$Proteins == proteinID,]

    proteomicsInput$Length <- nchar(proteomicsInput$Sequence)

    # Now Match the peptides and add the Start/Finish

    proteomicsInput$startPosition <- stringi::stri_locate(str = proteinSequence, regex = proteomicsInput$Sequence)[,1]

    proteomicsInput$endPosition <- stringi::stri_locate(str = proteinSequence, regex = proteomicsInput$Sequence)[,2]


    }


