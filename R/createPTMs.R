createPTMs <- function(evidence,
                        selectedProtein){




    modifiedPeptides <- evidence %>% dplyr::select(c(
        'Sequence', 'Modifications', 'Modified.sequence',
        'Proteins'
    ))


    # Index by Selected protein and remove Unmodified peptides

    # selectedProtein = 	'P10645'

    modifiedPeptides <- modifiedPeptides[modifiedPeptides$Proteins == selectedProtein,]

    modifiedPeptides <- modifiedPeptides[! modifiedPeptides$Modifications == 'Unmodified',]

    modifiedPeptides <- base::unique(modifiedPeptides)


    # In order to colour individually the modifications, only one modification
    # can exist per row. For those peptides containing multiple modificaitons:


    modifiedPeptides <-  modifiedPeptides %>% tidyr::separate_rows(Modifications, sep =  ',\\s?')

    # Remove the 1 , 2 , 3  and PTM since it's not needed.
    modifiedPeptides$Modifications <-   gsub('[1-3] ', '', modifiedPeptides$Modifications)


    for (ii in seq_len(nrow(modifiedPeptides))) {


      # Obtain the total modifications and add parenthesis: '(Oxidation (M))'
      Modifications <- base::paste0('(', base::unique(modifiedPeptides$Modifications), ')')



      # Modifications to remove, is the list of modifications withouth the
      # corresponding modification of that sequence

      modsToRemove <- Modifications[! Modifications %in%
                                  paste0('(',modifiedPeptides$Modifications[ii],')')]


      # Add back slash to the parentheses
      modsToRemove <- gsub(pattern = '\\(', replacement = '\\\\(', modsToRemove)

      modsToRemove <- gsub(pattern = '\\)', replacement = '\\\\)', modsToRemove)

      # Collapse the modifications to remove together to make it regex pattern

      modsToRemove <- paste(unlist(modsToRemove), collapse = '|')



      # Remove all the modifications for the modified sequence (except the one)
      # that is in the modifiedPeptides$Modification

      modifiedPeptides$Modified.sequence[ii] <- gsub(pattern = modsToRemove,'',
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


    # Remove the underscores:

    modifiedPeptides$Modified.sequence <-
        gsub(pattern = '_',
            replacement = '',
            x = modifiedPeptides$Modified.sequence)


   modifiedPeptides <-  unique(modifiedPeptides)


   # Remove 1 , 2 , 3 , from the column modification since they're redundant

   modifiedPeptides$Modifications <- gsub(pattern = '[1-3] ',
        replacement = '',
        x = modifiedPeptides$Modifications)

   return(modifiedPeptides)

}


