output$coverageUI <- renderUI({

    if (is.null(proteomicsInput()) ) {
        return(NULL)
    }
    req(input$inputComparison)


    column(
        width = 12,
        shinyWidgets::dropdown(

            selectInput("yaxisCoverage",
                        h4( "Select the Y-axis"),
                        choices = c("Log 2 Intensity" = "Intensity",
                                    "Peptide Start Position" = "startPosition",
                                    "Sequence" = 'sequence'),
                        selected = 'Intensity',
            ),

            selectInput("xAxisCoverage",
                        h4( "Select the X-axis"),
                        choices = c("Sequence" ='sequence',
                                    "Peptide Start Position" = "position"),
                        selected = "sequence"
            ),


            sliderInput('sizeSegments',
                        h4('Size of the Segments'),
                        min = 1,
                        max = 20,
                        value = 2),

            # shinyWidgets::switchInput('darkMode',
            #                           'Dark Mode', value = TRUE),
            shinyWidgets::colorPickr(inputId = 'backgroundColour',
                                     label = h4("Background Colour"),
                                     selected = "#333333"),

            sliderInput("intensityRange", label = h4("Intensity Log2 range"), min = 15,
                        max = 35, value = c(20, 33)
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

        if (input$inputComparison != 'conditions') {

            plotlyOutput('coveragePlot')
        }else{

            column(width = 12,
                   fluidRow(
                       column(width = 6,
                              plotlyOutput('coveragePlotComparisonOne')
                       ),
                       column(width = 6,
                              plotlyOutput('coveragePlotComparisonTwo')
                       )
                   )
            )
        }
    )
})
