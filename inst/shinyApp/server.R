function(input, output) {

    options(shiny.maxRequestSize=500*1024^2)## Set maximum upload size to 500MB


    proteomicsInput <- reactive({

        inFile <- input$proteomics_table


        #df <- read.delim(input$proteomics_table$datapath)

        if (is.null(inFile)) {
            return(NULL)
        } else if(! is.null(inFile)){

            df <- utils::read.delim(inFile$datapath)
        }

        return(df)
    })

    # proteomicsInput <- read.delim('inst/shinyApp/www/evidence.txt')

    #### Proteins to select ####

    proteinsToSelect <- reactive({


        proteinsToSelect <- proteomicsInput() %>%
            dplyr::select(
                c(
                    dplyr::contains('Proteins'),
                    - dplyr::contains('leading')
                )
            )

        proteinsToSelect <- base::unique(proteinsToSelect)

        return(proteinsToSelect)
    })


    output$proteinsSelect <- renderUI({

        if (is.null(proteinsToSelect())) {
            return(NULL)
        }else{

            shiny::selectInput(inputId = 'SelectedProtein',
                               label = 'Select a Protein of Interest',
                               choices = proteinsToSelect()$Proteins,
                               selected = proteinsToSelect()$Proteins[1])
        }

    })


    #message(nrow(proteomicsInput()))

        output$image <- renderUI({
            #tags$img(src = "http://wlab.ethz.ch/protter/create?up=P55011&tm=auto&mc=lightsalmon&lc=blue&tml=numcount&bc:yellow=C&bc:green=I&format=svg",width = 1010)

            tags$img(src = paste0("http://wlab.ethz.ch/protter/create?up=",input$SelectedProtein,"&tm=auto&mc=lightsalmon&lc=blue&tml=numcount&bc:yellow=C&bc:green=I&format=svg"),width = 1010)
        })


}

