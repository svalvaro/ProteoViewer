dfPeptidesColorsNoGroups <- reactive({

    shiny::req(input$inputComparison)

    if (is.null(proteomicsInput()) ||
        input$inputComparison == 'conditions') {
        return(NULL)
    }

    shiny::req(input$peptidesType)

    # Remove everything after the ":" in the proteinSelected
    # which is the description of the protein.

    proteinsSelected <- base::gsub("(.*):.*", "\\1", input$selectedProtein)

    # Create the peptides and colours tabular format
    # When plot_legend = FALSE, it returns a table containing
    # the peptides, and colors corresponding.

    dfPeptidesColorsNoGroups <- ProteoViewer::createLegend(
        evidence = proteomicsInput(),
        peptideType = input$peptidesType,
        experimentDesign = NULL,
        conditionSelected = NULL,
        selectedProtein = proteinsSelected,
        selectedExperiment = selectedExperiment(),
        comparison = input$inputComparison,
        plot_legend = FALSE)

    message(paste0('number of peptides:', nrow(dfPeptidesColorsNoGroups)))

    # If no peptides  for the given experiment
    if (is.null(dfPeptidesColorsNoGroups)) {
        return(NULL)
    } else{
        return(dfPeptidesColorsNoGroups)
    }
})
