function(input, output) {

    #### Upload the Proteomics Table ####

    ## Set maximum upload size to 500MB
    options(shiny.maxRequestSize=500*1024^2)



    proteomicsInput <- reactive({

        inFile <- input$proteomics_table

        if (is.null(inFile)) {
            return(NULL)
        } else if(! is.null(inFile)){

            df <- utils::read.delim(inFile$datapath)
        }

        #df <- read.delim('inst/shinyApp/www/evidence.txt')

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

            proteinsToSelect <- base::unique(proteomicsInput()$Proteins)

            shiny::selectInput(inputId = 'SelectedProtein',
                               label = 'Select a protein of interest',
                               choices = proteinsToSelect,
                               selected = proteinsToSelect[1])
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


    #### Render Image ####

        output$image <- renderUI({

            if (is.null(proteomicsInput())) {
                return(NULL)
            }

            # Create the url to connect to the API

            url <- ProteoViewer::connectProtterAPI(
                evidence = proteomicsInput(),
                SelectedProtein = input$SelectedProtein,
                SelectedExperiment = input$SelectedExperiment,
                combineExperiments = input$combineExperiments,
                plot_palette = FALSE
                )

            # render the image
            tags$img(src = url,
                  width = input$zoomFigure)
        })


    output$palette <- renderPlot(height = 100, width = 500,{
        ProteoViewer::connectProtterAPI(
            evidence = proteomicsInput(),
            SelectedProtein = input$SelectedProtein,
            SelectedExperiment = input$SelectedExperiment,
            combineExperiments = input$combineExperiments,
            plot_palette = TRUE)
    })

    #### Render the Legend ####
}

