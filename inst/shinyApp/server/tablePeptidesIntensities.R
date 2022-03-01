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

    # Add the peptide start and end

    uniprot_url <- "http://www.uniprot.org/uniprot/"

    uniprot_request <- paste0(uniprot_url, proteinsSelected, '.fasta')

    proteinSequence <- httr::GET(uniprot_request)

    proteinSequence   <- httr::content(proteinSequence, encoding = "UTF-8")

    # Keep only the sequence
    proteinSequence <- gsub(x = proteinSequence,
                            replacement = '',
                            pattern =  '.*.SV=.\n',
                            ignore.case = T)
    # Remove the \n
    proteinSequence <- gsub(x = proteinSequence,
                            replacement = '',
                            pattern = '\n',
                            ignore.case = T)

    # Now Match the peptides and add the Start/Finish

    peptideIntensityTable$`Start Position` <- stringi::stri_locate(str = proteinSequence, regex = peptideIntensityTable$Sequence)[,1]

    peptideIntensityTable$`End Position` <- stringi::stri_locate(str = proteinSequence, regex = peptideIntensityTable$Sequence)[,2]

    # Sort the table according to the start position

    peptideIntensityTable <- peptideIntensityTable[order(peptideIntensityTable$`Start Position` , decreasing = FALSE),]

    return(peptideIntensityTable)
})

output$peptideIntensityTableOut <- shiny::renderDataTable(options = list(pageLength= 10),
                                                          {
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
