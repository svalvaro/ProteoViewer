dfPeptidesColorsComparisonOne <- reactive({

    shiny::req(input$inputComparison == 'conditions')

    shiny::req(input$conditionsSelected[1])

    shiny::req(input$peptidesType)

    if (is.null(proteomicsInput())) {
        return(NULL)
    }

    if(!input$inputComparison == 'conditions'){
        return(NULL)
    }

    # Remove everything after the ":" in the proteinSelected
    # which is the description of the protein.

    proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )

    # Create the peptides and colours tabular format
    # When plot_legend = FALSE, it returns a table containing
    # the peptides, and colors corresponding.

    dfPeptidesColorsComparisonOne <- ProteoViewer::createLegend(
        evidence = proteomicsInput(),
        peptideType = input$peptidesType,
        experimentDesign = experimentDesignFinal$df,
        conditionSelected = input$conditionsSelected[1],
        selectedProtein = proteinsSelected,
        selectedExperiment = NULL,
        comparison = 'conditions',
        plot_legend = FALSE)

    # If no peptides  for the given experiment
    if (is.null(dfPeptidesColorsComparisonOne)) {
        return(NULL)
    } else{
        return(dfPeptidesColorsComparisonOne)
    }
})

proteinImageOne_url <- reactive({

    shiny::req(input$conditionsSelected[1])
    shiny::req(input$peptidesType)

    if (!input$inputComparison == 'conditions') {
        return(NULL)
    }
    # Remove everything after the ":" in the proteinSelected
    # which is the description of the protein.

    proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )

    # Obtain the PTM for the combined condition:

    # Obtain the PTMs table

    modifiedPeptides <- ProteoViewer::createPTMs(
        evidence = proteomicsInput(),
        peptideType = input$peptidesType,
        selectedProtein = proteinsSelected,
        selectedExperiment = NULL,
        experimentDesign = experimentDesignFinal$df,
        selectedCondition = input$conditionsSelected[1],
        plotLegend = FALSE
    )

    # Create the url to connect to the API

    url <- ProteoViewer::connectProtterAPI(
        dfPeptidesColors = dfPeptidesColorsComparisonOne(),
        selectedProtein = proteinsSelected,
        proteaseSelected = input$proteaseSelected,
        modifiedPeptides = modifiedPeptides,
        title = input$conditionsSelected[1]
    )

    return(url)

})

output$proteinImageComparisonOne <- renderImage({

    # Add error in case that the URL is too long.

    list(src = ProteoViewer::renderProtein(url = proteinImageOne_url()),
         width = 200)
},
deleteFile = TRUE)

# Title comparison
output$titleProteinComparisonOne <- renderText({

    paste0('Condition: ',  input$conditionsSelected[1])
})
