output$legendPTMs <- renderPlot(width = 300,{

    if (is.null(proteomicsInput())) {
        return(NULL)
    }

    shiny::req(input$peptidesType)

    # Remove everything after the ":" in the proteinSelected
    # which is the description of the protein.

    proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )

    modifiedPeptides <- ProteoViewer::createPTMs(
        evidence = proteomicsInput(),
        peptideType = input$peptidesType,
        selectedProtein = proteinsSelected,
        selectedExperiment = NULL,
        experimentDesign = NULL,
        selectedCondition = NULL,
        plotLegend = TRUE
    )
})
