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

    ## Set maximum upload size to 500MB
    options(shiny.maxRequestSize=500*1024^2)

    proteomicsInput <- reactive({

        inFile <- input$proteomics_table

        if(! is.null(inFile)){

            df <- utils::read.delim(inFile$datapath)

            # Select only the first uniprot in those ones containing multiple
            # Followed by a semicolon.

            # Repeated three times because sometimes there are multiple uniprot:
            # Q9BUB7;Q9BUB7;Q9BUB7;Q9BUB7
            df$Proteins <- base::gsub("(.*);.*", "\\1", df$Proteins)
            df$Proteins <- base::gsub("(.*);.*", "\\1", df$Proteins)
            df$Proteins <- base::gsub("(.*);.*", "\\1", df$Proteins)

        }else if(demo$start == TRUE){

            # evidence <- read.delim(system.file('shinyApp/www/evidence.txt', package = 'ProteoViewer'))

            df <- utils::read.delim(system.file('shinyApp/www/evidence.txt',
                                         package = 'ProteoViewer'))



            # Repeated three times because sometimes there are multiple uniprot:
            # Q9BUB7;Q9BUB7;Q9BUB7;Q9BUB7
            df$Proteins <- base::gsub("(.*);.*", "\\1", df$Proteins)
            df$Proteins <- base::gsub("(.*);.*", "\\1", df$Proteins)
            df$Proteins <- base::gsub("(.*);.*", "\\1", df$Proteins)

        } else if (is.null(inFile)) {
            return(NULL)

        }



        return(df)
    })

    # df <- read.delim('inst/shinyApp/www/evidence.txt')

    # proteomicsInput <- read.delim('inst/shinyApp/www/evidence.txt')%>%
    # dplyr::select(
    #     c(
    #         dplyr::contains(c('Proteins',
    #                           'Experiment',
    #                           'Protein.names',
    #                           'Sequence',
    #                           'Intensity')),
    #         - dplyr::contains(c('leading','max','modified'))))




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

            # Obtain only the uniques
            proteinsToSelect <- base::unique(proteinsToSelect)

            # Sort them alphabetically
            proteinsToSelect <- proteinsToSelect[
                base::order(proteinsToSelect$Display),]

            #proteinsToSelect <- base::unique(proteomicsInput()$Proteins)

            shiny::selectInput(inputId = 'SelectedProtein',
                               label = h4('Select a protein of interest'),
                               choices = proteinsToSelect$Display,
                               selected = proteinsToSelect$Display[1])
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


    # Output wether to combine or not the experiment. Don't show it if
    # THe compare between conditions is TRUE


    output$combineExperimentsOutput <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }


        if (!is.null(input$compareConditions) &&
            input$compareConditions == TRUE) {
            return(NULL)
        }

        checkboxInput('combineExperiments',
                      h4('Combine all the experiments'),
                      value = FALSE#,
                      #icon("paper-plane"),
                      #style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
    })

    output$experimentSelect <- renderUI({



          # if(!is.null(input$comparisonInput) & input$comparisonInput == TRUE) {
          #
          #       return(NULL)
          # }
          #
        if (is.null(proteomicsInput()) || input$combineExperiments == TRUE) {

              return(NULL)

        }

        if (!is.null(input$compareConditions) &&  input$compareConditions == TRUE) {
            return(NULL)
        }



            shiny::selectInput(inputId = 'SelectedExperiment',
                               label = h4('Select a Experiment'),
                               choices = experimentNames(),
                               selected = experimentNames()[1])

    })


    output$title_box <- renderText(input$SelectedProtein)



    #### Create table peptides & colours ####

    dfPeptidesColors <- reactive({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }



        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$SelectedProtein )



        # Create the peptides and colours tabular format
        # When plot_legend = FALSE, it returns a table containing
        # the peptides, and colors corresponding.

        dfPeptidesColors <- ProteoViewer::createLegend(
            evidence = proteomicsInput(),
            SelectedProtein = proteinsSelected,
            SelectedExperiment = input$SelectedExperiment,
            combineExperiments = input$combineExperiments,
            plot_legend = FALSE)



        # If no peptides  for the given experiment
        if (is.null(dfPeptidesColors)) {
            return(NULL)
        } else{
            return(dfPeptidesColors)
        }

    })



    #### Render proteinImage ####

    output$proteinImage <- renderUI({

        if (is.null(dfPeptidesColors())) {
            return(NULL)
        }

        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$SelectedProtein )


        # Create the url to connect to the API

        url <- ProteoViewer::connectProtterAPI(
            dfPeptidesColors = dfPeptidesColors(),
            SelectedProtein = proteinsSelected,
            proteaseSelected = input$proteaseSelected
            )

        # render the image
        shiny::tags$img(src = url,
              width = input$zoomFigure)
    })

    #### Render the Legend ####

    legend <- reactive({

        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.
        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$SelectedProtein )


        legend <- ProteoViewer::createLegend(
            evidence = proteomicsInput(),
            SelectedProtein = proteinsSelected,
            SelectedExperiment = input$SelectedExperiment,
            combineExperiments = input$combineExperiments,
            plot_legend = TRUE)


        if (is.null(legend) | is.null(dfPeptidesColors())) {
            return(NULL)
        }else{
            return(legend)
        }
    })


    output$noPeptidesErrorMessage <- renderText({

        if (is.null(dfPeptidesColors()) &
            ! is.null(proteomicsInput())) {
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



    output$comparisonCheck <- shiny::renderUI({

        if (is.null(experimentDesignFinal$df)) {
            return(NULL)
        }



        shiny::checkboxInput(inputId = 'compareConditions',
                      h4('Compare between conditions'),
                      value = FALSE
        )
    })


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
        if (is.null(experimentDesignFinal$df) |
            # Avoid error message if is null
            is.null(input$compareConditions)) {

            return(NULL)

        }else if(! is.null(input$compareConditions) &
                 input$compareConditions == FALSE){
            return(NULL)
        }

        # shinyWidgets::pickerInput(
        #     inputId = 'conditionsSelected',
        #     label = h4('Choose the groups that you would like to compare'),
        #     choices = comparisonsConditions(),
        #     # options = list(
        #     #     `actions-box` = TRUE,
        #     #     size = 10,
        #     #     `selected-text-format` = "count > 3"
        #     # ),
        #     multiple = TRUE
        #
        #     )


        shiny::selectInput(
                inputId = 'conditionsSelected',
                label = h4('Choose the groups that you would like to compare'),
                choices = comparisonsConditions(),
                multiple = TRUE
        )

    })


}

