proteinImage_url <- reactive({

    #req(input$inputComparison)

    if (input$inputComparison == 'conditions') {
        return(NULL)
    }
    # Remove everything after the ":" in the proteinSelected
    # which is the description of the protein.

    proteinsSelected <- base::gsub("(.*):.*", "\\1", input$selectedProtein )

    # Obtain the PTMs table

    modifiedPeptides <- ProteoViewer::createPTMs(
        evidence = proteomicsInput(),
        peptideType = input$peptidesType,
        selectedProtein = proteinsSelected,
        selectedExperiment = selectedExperiment(),
        experimentDesign = NULL,
        selectedCondition = NULL,
        plotLegend = FALSE
    )

    # Create the url to connect to the API

    url <- ProteoViewer::connectProtterAPI(
        dfPeptidesColors = dfPeptidesColorsNoGroups(),
        selectedProtein = proteinsSelected,
        proteaseSelected = input$proteaseSelected,
        modifiedPeptides = modifiedPeptides
    )

    return(url)

})

output$proteinImageNoComparison <- renderImage({

    if (input$inputComparison == 'conditions') {
        return(NULL)
    }

    url <- proteinImage_url()

    # Return a list containing the filename
    list(src = ProteoViewer::renderProtein(url = url),
         width = 200)
},
deleteFile = TRUE
)
