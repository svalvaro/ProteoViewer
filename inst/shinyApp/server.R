function(input, output) {

    #### Upload the Proteomics Table ####

    options(shiny.maxRequestSize=500*1024^2)## Set maximum upload size to 500MB


    proteomicsInput <- reactive({

        inFile <- input$proteomics_table




        if (is.null(inFile)) {
            return(NULL)
        } else if(! is.null(inFile)){

            df <- utils::read.delim(inFile$datapath) %>%
                dplyr::select(
                    c(
                        dplyr::contains(c('Proteins',
                                          'Experiment',
                                          'Protein.names',
                                          'Sequence',
                                          'Intensity')),
                        - dplyr::contains(c('leading',
                                            'max',
                                            'modified'))
                    )
                )


        }

        return(df)
    })

    # proteomicsInput <- read.delim('inst/shinyApp/www/evidence.txt')

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
                               label = 'Select a experiment',
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
                               label = 'Select a Protein of Interest',
                               choices = experimentToSelect,
                               selected = experimentToSelect[1])
        }

    })


    #### Proteomics table indexed by Experiment and Protein Selected ####


    ProteoIndexed <- reactive({

        df <- proteomicsInput()

        # Select only for the selected protein

        # ProteoIndexed <- df[df$Proteins == SelectedProtein,]
        ProteoIndexed <- df[df$Proteins == input$SelectedProtein,]



        if (input$combineExperiments == FALSE) {
            # ProteoIndexed <- df[df$Proteins == SelectedProtein,]
            ProteoIndexed <- ProteoIndexed[
                ProteoIndexed$Experiment == input$SelectedExperiment,]
        }


        # Remove rows containing NAs in the Intensity colum

        ProteoIndexed <- ProteoIndexed[!is.na(ProteoIndexed$Intensity),]



    })

    #  Number of peptides
    # output$text <- renderText(nrow(ProteoIndexed()))








    #### Render Image ####

        output$image <- renderUI({
            #tags$img(src = "http://wlab.ethz.ch/protter/create?up=P55011&tm=auto&mc=lightsalmon&lc=blue&tml=numcount&bc:yellow=C&bc:green=I&format=svg",width = 1010)

            tags$img(src = paste0("http://wlab.ethz.ch/protter/create?up=",input$SelectedProtein,"&tm=auto&mc=lightsalmon&lc=blue&tml=numcount&bc:yellow=C&bc:green=I&format=svg"),
                     width = input$zoomFiugre)
        })


}

