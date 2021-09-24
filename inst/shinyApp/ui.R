dashboardPage(
    dashboardHeader(title = "ProteoViewer",titleWidth =  300),
    dashboardSidebar(width = 300,
        # For now only evidence.txt from MaxQuant

        # Input: Select a file ----
        fileInput(inputId = "proteomics_table",
                  label =  h4("Upload the evidence.txt file"),
                  multiple = FALSE,
                  accept = 'text'),

        uiOutput('proteinsSelect'),


        checkboxInput('combineExperiments', h4('Combine the experiments'),
                      value = FALSE),

        uiOutput('experimentSelect'),


        checkboxInput('peptideCutter', h4('Show trypsin cutting sites'),
                      value = TRUE),

        sliderInput(inputId = 'zoomFigure',
                    label = h4('Select the size of the image'),
                    min = 100, max = 5000, value = 1000),


        # Load demo data.

        shinyWidgets::actionBttn(inputId = 'Demo',
                   label = 'Start Demo',
                   icon = NULL,
                   style = "unite",
                   color = "default",
                   size = "md",
                   block = FALSE,
                   no_outline = TRUE
        )

    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(

            #uiOutput(outputId = 'table'),

            box(title = h2(textOutput('title_box')),
                width = 1000,
                # Error message
                h4(textOutput('error_message')),
                uiOutput(outputId = 'image'),

                plotOutput('lenged')
                )
            )
        )
    )
