# Choose what the users wants to visualize: Individual experiments,
# all experiments combined, or conditions based on  experiment design

output$comparisonAll <- renderUI({

    if (is.null(proteomicsInput())) {
        return(NULL)
    }

    if (is.null(experimentDesignFinal$df)) {

        shinyWidgets::radioGroupButtons(
            inputId = "inputComparison",
            label = h4("What would you like to visualize?"),
            choices = c("Individual Experiments" = "individualExperiments",
                        "Combine all experiments together" = "combineExperiments"),
            status = "primary",
            direction = "vertical",
            size = 'normal'
        )

    }else{

        shinyWidgets::radioGroupButtons(
            inputId = "inputComparison",
            label = h4("What would you like to visualize?"),
            choices = c("Individual Experiments" = "individualExperiments",
                        "Combine all experiments together" = "combineExperiments",
                        "Conditions based on Exp. Design" = "conditions"),
            selected = 'conditions',
            status = "primary",
            direction = "vertical",
        )
    }
})
