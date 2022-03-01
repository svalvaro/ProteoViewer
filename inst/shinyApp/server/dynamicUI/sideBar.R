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
