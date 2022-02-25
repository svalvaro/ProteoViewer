coveragePlotReactive <- reactive({

    if (is.null(proteomicsInput()) || is.null(dfPeptidesColorsNoGroups())) {
        return(NULL)
    }

    req(input$selectedProtein)

    req(input$yaxisCoverage)

    req(input$sizeSegments)

    proteinsSelected <- base::gsub("(.*):.*", "\\1", input$selectedProtein)

    message("The protein Selected is: ", proteinsSelected)

    p <- ProteoViewer::proteinCoverage(proteinId = proteinsSelected,
                                       dfPeptidesColors = dfPeptidesColorsNoGroups(),
                                       yaxis = input$yaxisCoverage,
                                       sizeSegments = input$sizeSegments,
                                       backGroundColour = input$backgroundColour,
                                       intensityRange = input$intensityRange,
                                       xAxis = input$xAxisCoverage
    )
})

output$coveragePlot <- renderPlotly({
    if (is.null(coveragePlotReactive())) {
        return(NULL)
    }
    coveragePlotReactive()
})


coverageComparisonOne <- reactive({

    if (is.null(proteomicsInput()) || is.null(dfPeptidesColorsComparisonOne())) {
        stop()
    }

    req(input$selectedProtein)

    req(input$yaxisCoverage)

    req(input$sizeSegments)

    proteinsSelected <- base::gsub("(.*):.*", "\\1", input$selectedProtein)

    message("The protein Selected is: ", proteinsSelected)

    p <- ProteoViewer::proteinCoverage(proteinId = proteinsSelected,
                                       dfPeptidesColors = dfPeptidesColorsComparisonOne(),
                                       yaxis = input$yaxisCoverage,
                                       sizeSegments = input$sizeSegments,
                                       backGroundColour = input$backgroundColour,
                                       nameCondition = input$conditionsSelected[1],
                                       intensityRange = input$intensityRange,
                                       xAxis = input$xAxisCoverage
    )
    return(p)
})

coverageComparisonTwo <- reactive({

    if (is.null(proteomicsInput()) || is.null(dfPeptidesColorsComparisonTwo())) {
        stop()
    }

    req(input$selectedProtein)

    req(input$yaxisCoverage)

    req(input$sizeSegments)

    proteinsSelected <- base::gsub("(.*):.*", "\\1", input$selectedProtein)

    message("The protein Selected is: ", proteinsSelected)

    p <- ProteoViewer::proteinCoverage(proteinId = proteinsSelected,
                                       dfPeptidesColors = dfPeptidesColorsComparisonTwo(),
                                       yaxis = input$yaxisCoverage,
                                       sizeSegments = input$sizeSegments,
                                       backGroundColour = input$backgroundColour,
                                       nameCondition = input$conditionsSelected[2],
                                       intensityRange = input$intensityRange,
                                       xAxis = input$xAxisCoverage
    )
    return(p)

})

output$coveragePlotComparisonOne <- renderPlotly({
    coverageComparisonOne()
})

output$coveragePlotComparisonTwo <- renderPlotly({
    coverageComparisonTwo()
})
