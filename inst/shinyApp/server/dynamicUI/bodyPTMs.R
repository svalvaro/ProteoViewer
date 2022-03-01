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
