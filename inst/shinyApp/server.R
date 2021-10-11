function(input, output) {

    #### Load Demo Data ####

    demo <- shiny::reactiveValues(start = FALSE)


    observeEvent(input$Demo, {
        shiny::req(input$Demo)

        message(paste0('Demo is', demo$start))

        demo$start <-  TRUE

       shinyalert::shinyalert("Demo Data Loaded",
                               "Now you can visualize some proteins!",
                               type="success",
                               closeOnClickOutside = TRUE,
                               closeOnEsc = TRUE,
                               timer = 6000)
    })


    #### Upload the Proteomics Table ####

    ## Set maximum upload size to 1000MB
    options(shiny.maxRequestSize=1000*1024^2)

    proteomicsInput <- reactive({

        inFile <- input$proteomics_table

        if(! is.null(inFile)){

            df <- utils::read.delim(inFile$datapath)

            # Select only the first uniprot in those ones containing multiple
            # Followed by a semicolon.

            # Repeated three times because sometimes there are multiple uniprot:
            # Q9BUB7;Q9BUB7;Q9BUB7;Q9BUB7
            df$Proteins <- base::sub(";.*", "", df$Proteins)

        }else if(demo$start == TRUE){

            # evidence <- read.delim(system.file('shinyApp/www/evidence.txt', package = 'ProteoViewer'))
            # methyl evidence:
            # evidence <- read.delim('/run/user/1000/gvfs/afp-volume:host=FGU045NAS001.local,user=alvaro.sanchez,volume=045/ProteoLab_Projects/PLK_A_and_I/PLK057__A/PLK057__A-MQ__210817_Cunatova_LFQ_24x_Methyl/combined/txt/evidence.txt')

            df <- utils::read.delim(system.file('shinyApp/www/evidence.txt',
                                         package = 'ProteoViewer'))


            # Repeated three times because sometimes there are multiple uniprot:
            # Q9BUB7;Q9BUB7;Q9BUB7;Q9BUB7
            df$Proteins <- base::sub(";.*", "", df$Proteins)

        } else if (is.null(inFile)) {
            return(NULL)

        }



        return(df)
    })


    #### Proteins to select ####

    output$proteinsSelect <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }else{
            # Show also the protein name, create a column for that.

            proteinsToSelect <- proteomicsInput() %>% select(contains(
                c('Proteins', 'names')
            ))

            proteinsToSelect$Display <- paste0(
                proteinsToSelect$Proteins, ': ', proteinsToSelect$Protein.names
            )

             proteinsToSelect <- proteinsToSelect$Display

            # Obtain only the uniques
            proteinsToSelect <- base::unique(proteinsToSelect)

            # Sort them alphabetically
            proteinsToSelect <- proteinsToSelect[
                base::order(proteinsToSelect)]

            # Remove residual (':')

            proteinsToSelect <- proteinsToSelect[! proteinsToSelect %in% ': ']

            #proteinsToSelect <- base::unique(proteomicsInput()$Proteins)

            shiny::selectInput(inputId = 'selectedProtein',
                               label = h4('Select a protein of interest'),
                               choices = proteinsToSelect,
                               selected = proteinsToSelect[1])
        }

    })


    #### Experiment to select ####


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


        shiny::req(input$inputComparison == 'individualExperiments' )

        if (is.null(proteomicsInput()) ||
            !input$inputComparison == 'individualExperiments' ) {

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


    # Name of the protein
    output$title_box <- renderText(input$selectedProtein)


    output$comparisonAll <- renderUI({

        if (is.null(experimentDesignFinal$df)) {

            shinyWidgets::radioGroupButtons(
                inputId = "inputComparison",
                label = h4("What would you like to visualize?"),
                choices = c("Individual Experiments" = "individualExperiments",
                            "Combine all experiments together" = "combineExperiments"),

                status = "primary",
                direction = "vertical",
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



    #### Create table peptides & colours ####

    dfPeptidesColorsNoGroups <- reactive({

        if (is.null(proteomicsInput()) ||
            input$inputComparison == 'conditions') {
            return(NULL)
        }


        #shiny::req(input$inputComparison)



        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )



        # Create the peptides and colours tabular format
        # When plot_legend = FALSE, it returns a table containing
        # the peptides, and colors corresponding.

        dfPeptidesColorsNoGroups <- ProteoViewer::createLegend(
            evidence = proteomicsInput(),
            experimentDesign = NULL,
            conditionSelected = NULL,
            selectedProtein = proteinsSelected,
            selectedExperiment = input$selectedExperiment,
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



    #### Render proteinImage ####

    output$proteinImageNoComparison <- renderUI({

        if (is.null(dfPeptidesColorsNoGroups())) {
            return(NULL)
        }

        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )

        # Obtain the PTMs table

        modifiedPeptides <- ProteoViewer::createPTMs(
            evidence = proteomicsInput(),
            selectedProtein = proteinsSelected,
            selectedExperiment = input$selectedExperiment,
            experimentDesign = NULL,
            selectedCondition = NULL,
            plotLegend = FALSE
        )

        # Create the url to connect to the API

        url <- ProteoViewer::connectProtterAPI(
            dfPeptidesColors = dfPeptidesColorsNoGroups(),
            selectedProtein = proteinsSelected,
            proteaseSelected = input$proteaseSelected,
            modifiedPeptides = modifiedPeptides
            )

        # render the image
        shiny::tags$img(src = url,
              width = input$zoomFigure)
    })

    #### Legend PTMs ####


    output$legendPTMs <- renderPlot(width = 300,{

        if (is.null(dfPeptidesColorsNoGroups())) {
            return(NULL)
        }


        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )

        modifiedPeptides <- ProteoViewer::createPTMs(
            evidence = proteomicsInput(),
            selectedProtein = proteinsSelected,
            selectedExperiment = input$selectedExperiment,
            experimentDesign = NULL,
            selectedCondition = NULL,
            plotLegend = TRUE
        )


    })




    #### Render Comparison One ####


    dfPeptidesColorsComparisonOne <- reactive({

        shiny::req(input$inputComparison == 'conditions')

        shiny::req(input$conditionsSelected[1])

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        if(!input$inputComparison == 'conditions'){
            return(NULL)
        }

        # if (!is.null(input$compareConditions) &&
        #     input$compareConditions == FALSE) {
        #
        #     return(NULL)
        # }



        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )

        # Create the peptides and colours tabular format
        # When plot_legend = FALSE, it returns a table containing
        # the peptides, and colors corresponding.

        dfPeptidesColorsComparisonOne <- ProteoViewer::createLegend(
            evidence = proteomicsInput(),
            experimentDesign = experimentDesignFinal$df,
            conditionSelected = input$conditionsSelected[1],
            selectedProtein = proteinsSelected,
            selectedExperiment = NULL,
            comparison = 'conditions',
            plot_legend = FALSE)


        # If no peptides  for the given experiment
        if (is.null(dfPeptidesColorsComparisonOne)) {
            return(NULL)
        } else{
            return(dfPeptidesColorsComparisonOne)
        }

    })

    output$proteinImageComparisonOne <- renderUI({
        shiny::req(input$inputComparison == 'conditions')
        shiny::req(input$conditionsSelected[1])

        # if (is.null(dfPeptidesColorsComparisonOne())) {
        #     return(NULL)
        # }


        if (!input$inputComparison == 'conditions') {
            return(NULL)
        }
        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )


        # Obtain the PTM for the combined condition:

        # Obtain the PTMs table

        modifiedPeptides <- ProteoViewer::createPTMs(
            evidence = proteomicsInput(),
            selectedProtein = proteinsSelected,
            selectedExperiment = NULL,
            experimentDesign = experimentDesignFinal$df,
            selectedCondition = input$conditionsSelected[1],
            plotLegend = FALSE
        )


        # Create the url to connect to the API

        url <- ProteoViewer::connectProtterAPI(
            dfPeptidesColors = dfPeptidesColorsComparisonOne(),
            selectedProtein = proteinsSelected,
            proteaseSelected = input$proteaseSelected,
            modifiedPeptides = modifiedPeptides
            )

        # render the image
        shiny::tags$img(src = url,
                        width = input$zoomFigure)
    })



    output$titleProteinComparisonOne <- renderText({

        if (is.null(dfPeptidesColorsComparisonTwo())) {
            return(NULL)
        }

        paste0('Condition: ',  input$conditionsSelected[1])
    })

    #### Render Comparison Two ####


    dfPeptidesColorsComparisonTwo <- reactive({


        shiny::req(input$inputComparison == 'conditions')

        shiny::req(input$conditionsSelected[2])

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

    output$proteinImageComparisonTwo <- renderUI({

        # if (is.null(dfPeptidesColorsComparisonTwo())) {
        #     return(NULL)
        # }



        shiny::req(input$conditionsSelected[2])

        # if (!is.null(input$compareConditions) && input$compareConditions == FALSE) {
        #     return(NULL)
        # }


        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )


        # Obtain the PTMs table

        modifiedPeptides <- ProteoViewer::createPTMs(
            evidence = proteomicsInput(),
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
            modifiedPeptides = modifiedPeptides
            )

        # render the image
        shiny::tags$img(src = url,
                        width = input$zoomFigure)
    })


    output$titleProteinComparisonTwo <- renderText({

        if (is.null(dfPeptidesColorsComparisonTwo())) {
            return(NULL)
        }

        paste0('Condition: ',  input$conditionsSelected[2])
    })

    #### Render the Legend ####

    legend <- reactive({

        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.
        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )


        legend <- ProteoViewer::createLegend(
            evidence = proteomicsInput(),
            selectedProtein = proteinsSelected,
            selectedExperiment = input$selectedExperiment,
            comparison = NULL,
            plot_legend = TRUE)


        if (is.null(legend) | is.null(dfPeptidesColorsNoGroups())) {
            return(NULL)
        }else{
            return(legend)
        }
    })


    output$noPeptidesErrorMessage <- renderText({

        if (is.null(dfPeptidesColorsNoGroups()) &&
            ! is.null(proteomicsInput())
            ) {
            print('No peptides found for this experiment')
        }else{
            return(NULL)
        }
    })


    output$legend <- renderPlot(height = 100, width = 500,{
            legend()

    })


    #### Experiment Design ####


    experimentDesign <- reactive({

        if(! is.null(input$expDesignUpload)){

            experimentDesign <- utils::read.delim(
                input$expDesignUpload$datapath, header = TRUE)

            # If no experiment is provided but the user has uploaded the
            # evidence txt

        } else if(!is.null(input$proteomics_table)  ){



            experimentDesign <- data.frame(
                                        Experiment = experimentNames(),
                                        Condition = ' ',
                                        Replicate = as.numeric(' ')
                                        )

            # If the user starts the demo
            # For some reason input$Demo is 0 when False
            # then 1 when true, and if pressed multiple times, it continues to
            # increase. So for input$Demo >0  means when the user press demo
            # many times
        } else if(input$Demo > 0){

            experimentDesign <- utils::read.delim(
                system.file('shinyApp/www/experiment_design_example.txt',
                            package = 'ProteoViewer'),
                header = TRUE)
        }


        message(input$Demo)


        return(experimentDesign)

    })



    output$experimentDesignOutput <- rhandsontable::renderRHandsontable({


        if (is.null(experimentNames())) {
            return(NULL)
        }

        rhandsontable::rhandsontable(
            experimentDesign(),
            height =  500
            ) %>%
            rhandsontable::hot_col('Replicate', format = '0a') %>%
            rhandsontable::hot_col('Experiment', readOnly = TRUE)
    })



    # Obtain possible modifications that the user can make in the experiment
    # design

    experimentDesignFinal <- shiny::reactiveValues()

    observeEvent(input$experimentDesignOutput,{


            experimentDesignFinal$df <- rhandsontable::hot_to_r(
                input$experimentDesignOutput)

        print('The experiment design is:')
        print(experimentDesignFinal$df)

    })



    #### Comparisons regarding experiment design ####



    # output$comparisonCheck <- shiny::renderUI({
    #
    #     if (is.null(experimentDesignFinal$df)) {
    #         return(NULL)
    #     }
    #
    #
    #
    #     shiny::checkboxInput(inputId = 'compareConditions',
    #                   h4('Compare between conditions'),
    #                   value = FALSE
    #     )
    # })


    comparisonsConditions <- reactive({

        if (is.null(experimentDesignFinal$df)) {
            return(NULL)
        }

        comparisonsConditions <- base::sort(
            base::unique(
            experimentDesignFinal$df$Condition)
            )
    })


    output$comparisonSelector <- shiny::renderUI({

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


}

