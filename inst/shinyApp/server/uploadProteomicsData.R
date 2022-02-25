# Set maximum upload size to 1000MB
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

        df <- utils::read.delim(system.file('shinyApp/www/data/evidence.txt',
                                            package = 'ProteoViewer'))

        message('Demo data started')

        # Repeated three times because sometimes there are multiple uniprot:
        # Q9BUB7;Q9BUB7;Q9BUB7;Q9BUB7
        df$Proteins <- base::sub(";.*", "", df$Proteins)

    } else if (is.null(inFile)) {
        return(NULL)
    }

    return(df)
})
