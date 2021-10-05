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


    multiplePTMIndexes <- which(
      stringr::str_detect(modifiedPeptides$Modifications, ',')
      )


    df <- modifiedPeptides[multiplePTMIndexes,]


    df <-  df %>% tidyr::separate_rows(Modifications, sep =  ',\\s?')



    df$Modifications <-   gsub('[1-3] ', '', df$Modifications)


    #gsub('Acetyl \\(Protein N-term\\)','', df$Modified.sequence)


    for (ii in seq_len(nrow(df))) {


      Modifications <- c('(Acetyl (Protein N-term))','(Dimethyl (KR))','(Methyl (KR))')

      patterns <- Modifications[! Modifications %in% paste0('(',df$Modifications[ii],')')]

      patterns <- gsub(pattern = '\\(', replacement = '\\\\(', patterns)

      patterns <- gsub(pattern = '\\)', replacement = '\\\\)', patterns)

      print(patterns)

      df$Modified.sequence[ii] <- sapply(1:length(patterns),
                                         function(x) gsub(pattern = patterns[x],
                                                          '',
                                                          x = df$Modified.sequence[ii] ))
        #gsub(pattern = patterns,'', x = df$Modified.sequence[ii] )
        #reg(patterns, '', df$Modified.sequence[ii])

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


