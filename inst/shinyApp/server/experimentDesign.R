# Addition of a fourth column containing boolean values (TRUE/FALSE) to
# include that experiment or not.

experimentDesign <- reactive({

    if(!is.null(input$expDesignUpload)){

        experimentDesign <- utils::read.delim(
            input$expDesignUpload$datapath, header = TRUE)

        # If no experiment is provided but the user has uploaded the
        # evidence txt

        return(experimentDesign)

    } else if(!is.null(input$proteomics_table)  ){

        experimentDesign <- data.frame(
            label = experimentNames(),
            condition = '',
            replicate = NA_integer_,
            Include = TRUE
        )

        return(experimentDesign)

        # If the user starts the demo For some reason input$Demo is 0 when False
        # then 1 when true, and if pressed multiple times, it continues to
        # increase. So for input$Demo >0  means when the user press demo
        # many times

    } else if(input$Demo > 0){

        experimentDesign <- utils::read.delim(
            system.file('shinyApp/www/data/experiment_design_example.txt',
                        package = 'ProteoViewer'),
            header = TRUE)

        return(experimentDesign)
    }
})


expDesignEditable <- reactiveVal()

observeEvent(experimentDesign(),{

    if (is.null(proteomicsInput())) {
        return(NULL)
    }

    expDesignEditable(experimentDesign())

})

output$experimentDesignOutput <- renderRHandsontable({

    if (is.null(proteomicsInput())) {
        return(NULL)
    }

    df <- expDesignEditable()

    rhandsontable(
        df,
        height =  500) %>%
        hot_col('replicate', format = '0a') %>%

        rhandsontable::hot_col('label', readOnly = TRUE)%>%

        hot_table(highlightRow = TRUE) %>%

        hot_col(col = "Include", halign = 'htCenter',
                renderer = "
                    function (instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.CheckboxRenderer.apply(this, arguments);

                        var col_value = instance.getData()[row][3]

                        if (col_value === false) {

                            td.style.background = 'pink';
                        }
                    }
                ") %>%

        hot_col(col = c("label", "condition", "replicate"),halign = 'htCenter',
                renderer = "
                    function (instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);

                        var col_value = instance.getData()[row][3]

                        if (col_value === false) {

                            td.style.background = 'pink';
                        }
                    }
                ") %>%
        hot_col(col = 'replicate', readOnly = TRUE)

})

observeEvent(input$experimentDesignOutput, {
    userDT <- rhandsontable::hot_to_r(input$experimentDesignOutput)
    data.table::setDT(userDT)
    userDT[, replicate := seq_len(.N), by = condition][is.na(condition) | condition == "", replicate := NA_integer_]
    expDesignEditable(userDT)
})


experimentDesignFinal <- reactiveValues()

observeEvent(input$saveExpDesign, {

    experimentDesignFinal$df <-  rhandsontable::hot_to_r(input$experimentDesignOutput)

    # Remove the rows containing FALSE in include.

    experimentDesignFinal$df <- experimentDesignFinal$df [! experimentDesignFinal$df $Include == FALSE,]

    message('The experiment design is:')
    print(experimentDesignFinal$df )

})
