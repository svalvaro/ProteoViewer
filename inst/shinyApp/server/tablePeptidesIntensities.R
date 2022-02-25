peptideIntensityTable <- reactive({

    proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein)

    # Option 1: the user wants to see only one experiment

    if (input$inputComparison == 'individualExperiments') {

        selectedExperiment <- input$selectedExperiment

        peptideIntensityTable <- ProteoViewer::comparisonPTMs(
            evidence = proteomicsInput(),
            peptideType = input$peptidesType,
            selectedProtein = proteinsSelected,
            selectedExperiment = selectedExperiment,
            experimentDesign = NULL,
            selectedCondition = NULL
        )
    }

    # Option 2: the user wants to see the combination of experiments

    if (input$inputComparison == 'combineExperiments') {

        peptideIntensityTable <- ProteoViewer::comparisonPTMs(
            evidence = proteomicsInput(),
            peptideType = input$peptidesType,
            selectedProtein = proteinsSelected,
            selectedExperiment = NULL,
            experimentDesign = NULL,
            selectedCondition = NULL
        )
    }

    # Option 3: the user wants to see the comparison between conditions

    selectedConditions <- c( input$conditionsSelected[1],  input$conditionsSelected[2])

    if (input$inputComparison == 'conditions') {

        peptideIntensityTable <- ProteoViewer::comparisonPTMs(
            evidence = proteomicsInput(),
            peptideType = input$peptidesType,
            selectedProtein = proteinsSelected,
            selectedExperiment = NULL,
            experimentDesign = experimentDesignFinal$df,
            selectedConditions = selectedConditions
        )
    }

    return(peptideIntensityTable)
})

output$peptideIntensityTableOut <- shiny::renderDataTable({
    peptideIntensityTable()
})

# Download the table button

output$downloadTable <- downloadHandler(


    filename = function(){paste0(
        base::gsub("(.*):.*", "\\1",input$selectedProtein),
        '_protein_peptides_intensiteis.csv'
    )},
    content = function(fname){
        write.csv(peptideIntensityTable(), fname,row.names = FALSE)
    }
)
