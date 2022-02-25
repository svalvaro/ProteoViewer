dfPeptidesColorsComparisonTwo <- reactive({

    shiny::req(input$inputComparison == 'conditions')

    shiny::req(input$conditionsSelected[2])

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

    dfPeptidesColorsComparisonTwo <- ProteoViewer::createLegend(
        evidence = proteomicsInput(),
        peptideType = input$peptidesType,
        experimentDesign = experimentDesignFinal$df,
        conditionSelected = input$conditionsSelected[2],
        selectedProtein = proteinsSelected,
        selectedExperiment = NULL,
        comparison = 'conditions',
        plot_legend = FALSE)

    # If no peptides  for the given experiment
    if (is.null(dfPeptidesColorsComparisonTwo)) {
        return(NULL)
    } else{
        return(dfPeptidesColorsComparisonTwo)
    }
})

proteinImageTwo_url <- reactive({

    shiny::req(input$inputComparison == 'conditions')
    shiny::req(input$conditionsSelected[2])
    shiny::req(input$peptidesType)

    # Remove everything after the ":" in the proteinSelected
    # which is the description of the protein.

    proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )

    # Obtain the PTMs table

    modifiedPeptides <- ProteoViewer::createPTMs(
        evidence = proteomicsInput(),
        peptideType = input$peptidesType,
        selectedProtein = proteinsSelected,
        selectedExperiment = NULL,
        experimentDesign = experimentDesignFinal$df,
        selectedCondition = input$conditionsSelected[2],
        plotLegend = FALSE
    )

    # Create the url to connect to the API

    url <- ProteoViewer::connectProtterAPI(
        dfPeptidesColors = dfPeptidesColorsComparisonTwo(),
        selectedProtein = proteinsSelected,
        proteaseSelected = input$proteaseSelected,
        modifiedPeptides = modifiedPeptides,
        title = input$conditionsSelected[2]
    )
    return(url)
})

output$proteinImageComparisonTwo <- renderImage({

    list(src = ProteoViewer::renderProtein(url = proteinImageTwo_url()),
         width = 200)
},
deleteFile = TRUE)


output$titleProteinComparisonTwo <- renderText({

    paste0('Condition: ',  input$conditionsSelected[2])
})
