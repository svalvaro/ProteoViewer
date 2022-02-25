output$proteinsSelect <- renderUI({

    if (is.null(proteomicsInput())) {
        return(NULL)
    }else{

        # Remove proteins that peptides in the intensities, some proteins
        # are present but all their peptides have NAs in their
        # Intensity
        proteinsToSelect <- proteomicsInput()[!is.na(proteomicsInput()$Intensity),]

        # Show also the protein name, create a column for that.

        proteinsToSelect <- proteinsToSelect %>% select(contains(
            c('Proteins', 'names')
        ))


        proteinsToSelect$Display <- paste0(
            proteinsToSelect$Proteins, ': ', proteinsToSelect$Protein.names
        )

        proteinsToSelect <- proteinsToSelect$Display

        # Obtain only the uniques
        proteinsToSelect <- base::unique(proteinsToSelect)

        # Sort them alphabetically
        proteinsToSelect <- proteinsToSelect[
            base::order(proteinsToSelect)]

        # Remove residual (':')

        proteinsToSelect <- proteinsToSelect[! proteinsToSelect %in% ': ']

        #proteinsToSelect <- base::unique(proteomicsInput()$Proteins)

        shiny::selectInput(inputId = 'selectedProtein',
                           label = h4('Select a protein of interest'),
                           choices = proteinsToSelect,
                           selected = proteinsToSelect[1])
    }
})
