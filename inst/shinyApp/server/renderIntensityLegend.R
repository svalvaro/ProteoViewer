legend <- reactive({

    # Remove everything after the ":" in the proteinSelected
    # which is the description of the protein.
    proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein)

    legend <- ProteoViewer::createLegend(
        evidence = proteomicsInput(),
        peptideType = 'Both',
        selectedProtein = proteinsSelected,
        selectedExperiment = selectedExperiment(),
        comparison = NULL,
        plot_legend = TRUE)

    if (is.null(legend)) {
        return(NULL)
    }
    return(legend)
})

output$noPeptidesErrorMessage <- renderText({

    if (is.null(dfPeptidesColorsNoGroups()) &&
        (!is.null(proteomicsInput()) && (input$inputComparison != 'conditions'))
    ) {
        print('No peptides found for this experiment')
    }else{
        return(NULL)
    }
})

output$legend <- renderPlot(height = 100, width = 500,{
    legend()
})
