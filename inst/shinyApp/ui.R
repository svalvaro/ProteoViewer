dashboardPage(
    dashboardHeader(title = "ProteoViewer"),
    dashboardSidebar(
        # For now only evidence.txt from MaxQuant

        # Input: Select a file ----
        fileInput(inputId = "proteomics_table",
                  label =  "Upload the evidence.txt file",
                  multiple = FALSE,
                  accept = 'text'),

        uiOutput('proteinsSelect'),


        checkboxInput('combineExperiments', 'Combine the experiments',
                      value = FALSE),

        uiOutput('experimentSelect'),


        checkboxInput('peptideCutter', 'Show trypsin cutting sites',
                      value = TRUE),

        sliderInput(inputId = 'zoomFigure',
                    label = 'Select the size of the image',
                    min = 100, max = 5000, value = 1000)
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
