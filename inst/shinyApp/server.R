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

            #df <- read.delim('www/evidence.txt')

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

            proteinsToSelect <- unique(proteinsToSelect)

            #proteinsToSelect <- base::unique(proteomicsInput()$Proteins)

            shiny::selectInput(inputId = 'SelectedProtein',
                               label = 'Select a protein of interest',
                               choices = proteinsToSelect$Display,
                               selected = proteinsToSelect$Display[1])
        }

    })


    #### Experiment to select ####

    output$experimentSelect <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }else if (input$combineExperiments == TRUE){
            return(NULL)
        }else{

            experimentToSelect <- base::unique(proteomicsInput()$Experiment)

            shiny::selectInput(inputId = 'SelectedExperiment',
                               label = 'Select a Experiment',
                               choices = experimentToSelect,
                               selected = experimentToSelect[1])
        }

    })


    output$title_box <- renderText(input$SelectedProtein)

    #### Render Image ####

        output$image <- renderUI({

            if (is.null(proteomicsInput())) {
                return(NULL)
            }

            # Remove everything after the ":" in the proteinSelected
            # which is the description of the protein.

            proteinsSelected <- base::gsub("(.*):.*", "\\1",input$SelectedProtein )

            # Create the url to connect to the API

            url <- ProteoViewer::connectProtterAPI(
                evidence = proteomicsInput(),
                SelectedProtein = proteinsSelected,
                SelectedExperiment = input$SelectedExperiment,
                combineExperiments = input$combineExperiments,
                peptideCutter = input$peptideCutter,
                plot_palette = FALSE
                )

            # render the image
            shiny::tags$img(src = url,
                  width = input$zoomFigure)
        })


    legend <- reactive({
        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$SelectedProtein )

        legend <- ProteoViewer::connectProtterAPI(
            evidence = proteomicsInput(),
            SelectedProtein = proteinsSelected,
            SelectedExperiment = input$SelectedExperiment,
            combineExperiments = input$combineExperiments,
            plot_palette = TRUE)


        if (is.null(legend)) {
            return(NULL)
        }else{
            return(legend)
        }
    })


    output$error_message <- renderText({

        if (is.null(legend()) &
            ! is.null(proteomicsInput())) {
            print('No peptides found for this experiment')
        }else{
            return(NULL)
        }
    })
    #### Render the Legend ####
    output$lenged <- renderPlot(height = 100, width = 500,{
            legend()

    })


}

