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
                imageOutput(outputId = 'proteinImageComparisonOne'),
                tags$script(HTML('panzoom($("#proteinImageComparisonOne").get(0))'))
            ),

            div(
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
