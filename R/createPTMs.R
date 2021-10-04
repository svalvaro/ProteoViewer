createPTMs <- function(evidence,
                        selectedProtein){




    modifiedPeptides <- evidence %>% dplyr::select(c(
        'Sequence', 'Modifications', 'Modified.sequence',
        'Proteins'
    ))


    # Index by Selected protein and remove Unmodified peptides

    # selectedProtein = 	'P56134'

    modifiedPeptides <- modifiedPeptides[modifiedPeptides$Proteins == selectedProtein,]

    modifiedPeptides <- modifiedPeptides[! modifiedPeptides$Modifications == 'Unmodified',]


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
             x = modifiedPeptides$Modified.sequence)



    # Remove the underscores:

    modifiedPeptides$Modified.sequence <-
        gsub(pattern = '_',
            replacement = '',
            x = modifiedPeptides$Modified.sequence)


   modifiedPeptides <-  unique(modifiedPeptides)

}
