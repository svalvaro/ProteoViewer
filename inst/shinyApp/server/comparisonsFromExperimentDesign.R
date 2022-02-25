comparisonsConditions <- reactive({

    if (is.null(experimentDesignFinal$df)) {
        return(NULL)
    }

    comparisonsConditions <- base::sort(
        base::unique(
            experimentDesignFinal$df$condition)
    )
})

output$comparisonSelector <- shiny::renderUI({

    req(input$inputComparison)
    # If no experiment design is provided
    if (is.null(experimentDesignFinal$df) ||
        !input$inputComparison == 'conditions'){

        return(NULL)
    }

    if(input$inputComparison == 'conditions'){

        colors <- base::rep('color:black;background:white',
                            length(comparisonsConditions()))

        shinyWidgets::pickerInput(
            inputId = 'conditionsSelected',
            label = h4('Choose the groups that you would like to compare'),
            choices = comparisonsConditions(),
            multiple = TRUE,
            selected = comparisonsConditions()[c(1,2)],
            options = list("max-options" = 2),
            choicesOpt = list(style= colors)
        )
    }
})
