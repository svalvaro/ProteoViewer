function(input, output) {

    #### Load Demo Data  & Notifications####

    demo <- shiny::reactiveValues(start = FALSE)

    observeEvent(input$Demo, {
        shiny::req(input$Demo)

        demo$start <-  TRUE

        message('Demo data being loaded')

       shinyalert::shinyalert("Demo Data Loaded",
                               "Now you can visualize some proteins!",
                               type="success",
                               closeOnClickOutside = TRUE,
                               closeOnEsc = TRUE,
                               timer = 6000)
    })

    observeEvent(input$saveExpDesign, {

        message('Experiment Design updated')


        shinyalert::shinyalert("Experiment Design saved",
                               "You can go back to the Results Tab",
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

    #### Proteins to select ####

    output$proteinsSelect <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }else{

            # Remove proteins that peptides in the intensities, some proteins
            # are present but all their peptides have NAs in their
            # Intensity
            proteinsToSelect <- proteomicsInput()[!is.na(proteomicsInput()$Intensity),]

            # Show also the protein name, create a column for that.

            proteinsToSelect <- proteinsToSelect %>% select(contains(
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

        if (is.null(proteomicsInput()) ||
            is.null(input$inputComparison) ) {

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

    # Make the selected Experiment reactive, that way it will be NULL when
    # is combineExperiments or conditions are selected.

    selectedExperiment <- reactive({
        req(input$inputComparison)

        if (input$inputComparison != 'individualExperiments' ) {
            selectedExperiment <- NULL
        }else{
            selectedExperiment <- input$selectedExperiment
        }
        return(selectedExperiment)
    })


    # Name of the protein on top of the plot

    output$title_box <- renderText(input$selectedProtein)

    #### Comparison Selector ####
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
                # size = 'large' adjust the rest to make it a bit bigger
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

    #### Create table Protein No Conditions ####

    dfPeptidesColorsNoGroups <- reactive({


        shiny::req(input$inputComparison)

        if (is.null(proteomicsInput()) ||
            input$inputComparison == 'conditions') {
            return(NULL)
        }

        shiny::req(input$peptidesType)

        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1", input$selectedProtein)

        # Create the peptides and colours tabular format
        # When plot_legend = FALSE, it returns a table containing
        # the peptides, and colors corresponding.

        dfPeptidesColorsNoGroups <- ProteoViewer::createLegend(
            evidence = proteomicsInput(),
            peptideType = input$peptidesType,
            experimentDesign = NULL,
            conditionSelected = NULL,
            selectedProtein = proteinsSelected,
            selectedExperiment = selectedExperiment(),
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


    proteinImage_url <- reactive({

        #req(input$inputComparison)

        if (input$inputComparison == 'conditions') {
            return(NULL)
        }
        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1", input$selectedProtein )

        # Obtain the PTMs table

        modifiedPeptides <- ProteoViewer::createPTMs(
            evidence = proteomicsInput(),
            peptideType = input$peptidesType,
            selectedProtein = proteinsSelected,
            selectedExperiment = selectedExperiment(),
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

        return(url)

    })

    output$proteinImageNoComparison <- renderImage({

        if (input$inputComparison == 'conditions') {
            stop()
        }

        url <- proteinImage_url()

        # Return a list containing the filename
        list(src = ProteoViewer::renderProtein(url = url),
             width = 200)
        },
        deleteFile = TRUE
        )


    #### Legend PTMs ####

    output$legendPTMs <- renderPlot(width = 300,{

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        shiny::req(input$peptidesType)

        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )

        modifiedPeptides <- ProteoViewer::createPTMs(
            evidence = proteomicsInput(),
            peptideType = input$peptidesType,
            selectedProtein = proteinsSelected,
            selectedExperiment = NULL,
            experimentDesign = NULL,
            selectedCondition = NULL,
            plotLegend = TRUE
        )
    })

    #### Render Comparison One ####

    dfPeptidesColorsComparisonOne <- reactive({

        shiny::req(input$inputComparison == 'conditions')

        shiny::req(input$conditionsSelected[1])

        shiny::req(input$peptidesType)

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

        dfPeptidesColorsComparisonOne <- ProteoViewer::createLegend(
            evidence = proteomicsInput(),
            peptideType = input$peptidesType,
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


    proteinImageOne_url <- reactive({

        shiny::req(input$conditionsSelected[1])
        shiny::req(input$peptidesType)

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
            peptideType = input$peptidesType,
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
            modifiedPeptides = modifiedPeptides,
            title = input$conditionsSelected[1]
        )

        return(url)

    })

    output$proteinImageComparisonOne <- renderImage({

        # Add error in case that the URL is too long.

        list(src = ProteoViewer::renderProtein(url = proteinImageOne_url()),
             width = 200)
    },
    deleteFile = TRUE)

    # Title comparison
    output$titleProteinComparisonOne <- renderText({

        paste0('Condition: ',  input$conditionsSelected[1])
    })

    #### Render Comparison Two ####


    dfPeptidesColorsComparisonTwo <- reactive({

        shiny::req(input$inputComparison == 'conditions')

        shiny::req(input$conditionsSelected[2])

        shiny::req(input$peptidesType)

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
            peptideType = input$peptidesType,
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

    proteinImageTwo_url <- reactive({

        shiny::req(input$inputComparison == 'conditions')
        shiny::req(input$conditionsSelected[2])
        shiny::req(input$peptidesType)

        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.

        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein )

        # Obtain the PTMs table

        modifiedPeptides <- ProteoViewer::createPTMs(
            evidence = proteomicsInput(),
            peptideType = input$peptidesType,
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
            modifiedPeptides = modifiedPeptides,
            title = input$conditionsSelected[2]
        )
        return(url)
    })

    output$proteinImageComparisonTwo <- renderImage({

        list(src = ProteoViewer::renderProtein(url = proteinImageTwo_url()),
             width = 200)
    },
    deleteFile = TRUE)


    output$titleProteinComparisonTwo <- renderText({

        paste0('Condition: ',  input$conditionsSelected[2])
    })

    #### Render the Legend ####

    legend <- reactive({

        # Remove everything after the ":" in the proteinSelected
        # which is the description of the protein.
        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein)

        legend <- ProteoViewer::createLegend(
            evidence = proteomicsInput(),
            peptideType = 'Both',
            selectedProtein = proteinsSelected,
            selectedExperiment = selectedExperiment(),
            comparison = NULL,
            plot_legend = TRUE)

        if (is.null(legend)) {
            return(NULL)
        }
        return(legend)
    })

    output$noPeptidesErrorMessage <- renderText({

        if (is.null(dfPeptidesColorsNoGroups()) &&
            (!is.null(proteomicsInput()) && (input$inputComparison != 'conditions'))
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

    #### Comparisons regarding experiment design ####

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

    #### Table with peptide intensities ####

    peptideIntensityTable <- reactive({

        proteinsSelected <- base::gsub("(.*):.*", "\\1",input$selectedProtein)

        # Option 1: the user wants to see only one experiment

        if (input$inputComparison == 'individualExperiments') {

            selectedExperiment <- input$selectedExperiment

            peptideIntensityTable <- ProteoViewer::comparisonPTMs(
                evidence = proteomicsInput(),
                peptideType = input$peptidesType,
                selectedProtein = proteinsSelected,
                selectedExperiment = selectedExperiment,
                experimentDesign = NULL,
                selectedCondition = NULL
            )
        }

        # Option 2: the user wants to see the combination of experiments

        if (input$inputComparison == 'combineExperiments') {

            peptideIntensityTable <- ProteoViewer::comparisonPTMs(
                evidence = proteomicsInput(),
                peptideType = input$peptidesType,
                selectedProtein = proteinsSelected,
                selectedExperiment = NULL,
                experimentDesign = NULL,
                selectedCondition = NULL
            )
        }

        # Option 3: the user wants to see the comparison between conditions

        selectedConditions <- c( input$conditionsSelected[1],  input$conditionsSelected[2])

        if (input$inputComparison == 'conditions') {

            peptideIntensityTable <- ProteoViewer::comparisonPTMs(
                evidence = proteomicsInput(),
                peptideType = input$peptidesType,
                selectedProtein = proteinsSelected,
                selectedExperiment = NULL,
                experimentDesign = experimentDesignFinal$df,
                selectedConditions = selectedConditions
            )
        }

        return(peptideIntensityTable)
    })

    output$peptideIntensityTableOut <- shiny::renderDataTable({
        peptideIntensityTable()
    })

    # Download the table button

    output$downloadTable <- downloadHandler(


        filename = function(){paste0(
            base::gsub("(.*):.*", "\\1",input$selectedProtein),
            '_protein_peptides_intensiteis.csv'
        )},
        content = function(fname){
            write.csv(peptideIntensityTable(), fname,row.names = FALSE)
        }
    )


    #### Plot Coverage ####

    coveragePlotReactive <- reactive({

        if (is.null(proteomicsInput()) || is.null(dfPeptidesColorsNoGroups())) {
            stop()
        }

        req(input$selectedProtein)

        req(input$yaxisCoverage)

        proteinsSelected <- base::gsub("(.*):.*", "\\1", input$selectedProtein)

        message("The protein Selected is: ", proteinsSelected)

        p <- ProteoViewer::proteinCoverage(proteinId = proteinsSelected,
                                    dfPeptidesColors = dfPeptidesColorsNoGroups(),
                                    yaxis = input$yaxisCoverage)
    })

    output$coveragePlot <- renderPlotly({
        coveragePlotReactive()
    })

    #### User Interface Reactive ####

        # Peptides type ----------------

    output$peptidesIntensitySelector <- renderUI({
        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        shinyWidgets::radioGroupButtons(
            inputId = "peptidesType",
            label = h4("Type of peptides to visualize?"),
            choices = c("Unmodified", "Modified", "Both"),
            status = "primary",
            selected = 'Both',
        )
    })

        # Protease ------------------

    output$proteaseSelector <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        selectInput(
            inputId = 'proteaseSelected',

            label =  h4('Protease'),

            choices = c(
                'No protease' = 'none',
                'Trypsin' = 'Tryps',
                'LysC' = 'LysC',
                'LysN' = 'LysN',
                'Arg-C proteinase' = 'Arg-Cproteinase',
                'Asp-N endopeptidase' = 'Asp-Nendopeptidase',
                'Asp-N endopeptidase + N-terminal Glu' = 'Asp-Nendopeptidase+N-terminalGlu',
                'BNPS-Skatole' = 'BNPS-Skatole',
                'Caspase1' = 'Caspase1',
                'Caspase2' = 'Caspase2',
                'Caspase3' = 'Caspase3',
                'Caspase4' = 'Caspase4',
                'Caspase5' = 'Caspase5',
                'Caspase6' = 'Caspase6',
                'Caspase7' = 'Caspase7',
                'Caspase8' = 'Caspase8',
                'Caspase9' = 'Caspase9',
                'Caspase10' = 'Caspase10',
                'Chymotrypsin-high specificity' = 'Chymotrypsin-highspecificity',
                'Chymotrypsin-low specificity' = 'Chymotrypsin-lowspecificity',
                'Clostripain (Clostridiopeptidase B)' = 'Clostripain(ClostridiopeptidaseB)',
                'CNBr' = 'CNBr',
                'Enterokinase' = 'Enterokinase',
                'Factor Xa' = 'FactorXa',
                'Formic acid' = 'Formicacid',
                'Glutamyl endopeptidase' = 'Glutamylendopeptidase',
                'GranzymeB' = 'GranzymeB',
                'Hydroxylamine' = 'Hydroxylamine',
                'Iodosobenzoic acid' = 'Iodosobenzoicacid',
                'NTCB (2-nitro-5-thiocyanobenzoic acid)' = 'NTCB(2-nitro-5-thiocyanobenzoicacid)',
                'Pepsin (pH1.3)' = 'Pepsin(pH1.3)',
                'Pepsin (pH>2)' = 'Pepsin(pH>2)',
                'Proline-endopeptidase' = 'Proline-endopeptidase',
                'Proteinase K' = 'ProteinaseK',
                'Staphylococcal peptidase I' = 'StaphylococcalpeptidaseI',
                'Tobacco etch virus protease' = 'Tobaccoetchvirusprotease',
                'Thermolysin' = 'Thermolysin',
                'Thrombin' = 'Thrombin'),

            selected = 'none')
    })


        # Downloader for the pictures ----------------


    noComparisonURL <- reactive({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        url <- gsub(pattern = "format=svg",
                    replacement = "format=png",
                    proteinImage_url())

        message("url no comparison")
        message(url)

        return(url)
    })

    output$singleDownload <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        if (input$inputComparison != 'individualExperiments') {
            return(NULL)
        }

        shiny::a(
            actionBttn(
                inputId = 'downloadPNG1',
                label = 'Download Protein',
                icon = icon("download"),
                style = "unite",
                color = "default",
                size = "md",
                block = FALSE,
                no_outline = TRUE
                ),
            target = "_blank",
            href = noComparisonURL()
        )

    })


    output$combineDownload <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        if (input$inputComparison != 'combineExperiments') {
            return(NULL)
        }

        shiny::a(
            actionBttn(
                inputId = 'downloadPNG2',
                label = 'Download Protein',
                icon = icon("download"),
                style = "unite",
                color = "default",
                size = "md",
                block = FALSE,
                no_outline = TRUE
            ),
            target = "_blank",
            href = noComparisonURL()
        )

    })






    output$downloadComparisonOne <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        if (input$inputComparison != 'conditions') {
            return(NULL)
        }

        shiny::req(input$conditionsSelected[1])

        condtionSelected <- input$conditionsSelected[1]

        url <- gsub(pattern = "format=svg",
                    replacement = "format=png",
                    proteinImageOne_url())

        shiny::a(
            actionBttn(
                inputId = 'downloadPNG',
                label = paste0('Download Condition: ', condtionSelected),
                icon = icon("download"),
                style = "unite",
                color = "default",
                size = "md",
                block = FALSE,
                no_outline = TRUE
            ),
            target = "_blank",
            href = url)
    })


    output$downloadComparisonTwo <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        if (input$inputComparison != 'conditions') {
            return(NULL)
        }

        shiny::req(input$conditionsSelected[2])

        condtionSelected <- input$conditionsSelected[2]

        url <- gsub(pattern = "format=svg",
                    replacement = "format=png",
                    proteinImageTwo_url())

        shiny::a(
            actionBttn(
                inputId = 'downloadPNG',
                label = paste0('Download Condition: ', condtionSelected),
                icon = icon("download"),
                style = "unite",
                color = "default",
                size = "md",
                block = FALSE,
                no_outline = TRUE
            ),
            target = "_blank",
            href = url)
    })

    output$downloadersUI <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        req(input$inputComparison)


        if (input$inputComparison != 'conditions') {

            div(
                uiOutput("singleDownload"),
                uiOutput("combineDownload")

            )

        }else{
          div(
              uiOutput("downloadComparisonOne"),

              uiOutput("downloadComparisonTwo")
          )
        }
    })


        # UI protein images ------------------
    output$UserInterNoGroups <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        if(input$inputComparison == 'conditions'){
            return(NULL)
        }

        div(
            h2(textOutput('title_box')),

            h3(textOutput('noPeptidesErrorMessage')),

            imageOutput(outputId = 'proteinImageNoComparison'),

            tags$script(HTML('panzoom($(".shiny-image-output").get(0))'))

        )
    })



    output$UserInterGroups <- renderUI({

        req(input$inputComparison)

        if (input$inputComparison != 'conditions') {
            return(NULL)
        }

        fluidRow(
            column(
                width = 12,

                div(
                    #h3(textOutput('titleProteinComparisonOne')),
                    imageOutput(outputId = 'proteinImageComparisonOne'),
                    tags$script(HTML('panzoom($("#proteinImageComparisonOne").get(0))'))
                ),

                div(
                    #h3(textOutput('titleProteinComparisonTwo')),
                    imageOutput(outputId = 'proteinImageComparisonTwo'),
                    tags$script(HTML('panzoom($("#proteinImageComparisonTwo").get(0))'))
                )
            )
        )
    })

    output$proteinImagesUIAll <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        column(
            width = 12,

            h2(textOutput('title_box')),

            uiOutput('UserInterNoGroups'),

            uiOutput('UserInterGroups')
        )
    })



        # UI Coverage -----------------------

    output$coverageUI <- renderUI({

        if (is.null(proteomicsInput()) ) {
            return(NULL)
        }

         req(!is.null(dfPeptidesColorsNoGroups()))
        #
        # if (is.null(dfPeptidesColorsNoGroups())) {
        #     stop()
        # }


        column(
            width = 12,
            shinyWidgets::dropdown(

                selectInput("yaxisCoverage",
                            "Select the y axis",
                            choices = c("Log 2 Intensity" = "Intensity",
                                        "Peptide Start Position" = "startPosition"),
                ),

                #options = list(`style` = "btn-info"),
                style = "unite",
                icon = tags$i(
                    class = "fa fa-gear",
                    style = "color: rgb(255,255,255)"
                ),
                status = "success",
                width = "300px",
                animate = animateOptions(
                    enter = animations$fading_entrances$fadeInLeftBig,
                    exit = animations$fading_exits$fadeOutRightBig)
            ),
            plotlyOutput('coveragePlot')
        )


    })


    shinyWidgets::dropdown(

        selectInput("yaxisCoverage",
                    "Select the y axis",
                    choices = c("Log 2 Intensity" = "Intensity",
                                "Peptide Start Position" = "startPosition"),
        ),

        options = list(`style` = "btn-info"),
        style = "unite",
        icon = icon("paint-brush"),
        status = "success", width = "300px",
        animate = animateOptions(
            enter = animations$fading_entrances$fadeInLeftBig,
            exit = animations$fading_exits$fadeOutRightBig)
    )
    plotlyOutput('coveragePlot')

        # UI PTMS ---------------------

    output$PTMSlegendUI <- renderUI({

        if (is.null(proteomicsInput())) {
            return(NULL)
        }

        column(
            width = 12,
            box(width = 400,
                plotOutput('legendPTMs'),

                dataTableOutput('peptideIntensityTableOut'),
                downloadButton(outputId = 'downloadTable',
                               label = 'Download')
                )
            )
    })


    output$legendIntensities <- renderUI({

        if (is.null(legend())) {
            return(NULL)
        }
        box(
            height = '120px',
            width = '100px',
            plotOutput('legend')
        )
    })
}
