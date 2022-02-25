# First we need the experiment names.

experimentNames <- reactive({

    if (is.null(proteomicsInput())) {
        return(NULL)
    }

    experimentNames <- base::sort(
        base::unique(proteomicsInput()$Experiment)
    )
    return(experimentNames)
})

output$experimentSelect <- renderUI({

    if (is.null(proteomicsInput()) ||
        is.null(input$inputComparison) ) {

        return(NULL)
    }

    if (input$inputComparison == 'individualExperiments' ) {

        shiny::selectInput(
            inputId = 'selectedExperiment',
            label = h4('Select a Experiment'),
            choices = experimentNames(),
            selected = experimentNames()[1])
    }
})

# Make the selected Experiment reactive, that way it will be NULL when
# is combineExperiments or conditions are selected.

selectedExperiment <- reactive({
    req(input$inputComparison)

    if (input$inputComparison != 'individualExperiments' ) {
        selectedExperiment <- NULL
    }else{
        selectedExperiment <- input$selectedExperiment
    }
    return(selectedExperiment)
})


# Name of the protein on top of the plot

output$title_box <- renderText(input$selectedProtein)
