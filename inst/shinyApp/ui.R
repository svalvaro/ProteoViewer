dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(

        # Upload the proteomics txt file.
        # For now only evidence.txt from MaxQuant

        # Input: Select a file ----
        fileInput(inputId = "proteomics_table",
                  label =  "Upload the evidence.txt file",
                  multiple = FALSE,
                  accept = 'text'),

        verbatimTextOutput("value"),

        uiOutput('proteinsSelect'),


        checkboxInput('combineExperiments', 'Combine the experiments',
                      value = FALSE),

        uiOutput('experimentSelect'),


        #textOutput('text'),


        sliderInput(inputId = 'zoomFigure',label = 'Select the size of the image',
                    min = 500, max = 10000, value = 1000)
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(

            #uiOutput(outputId = 'table'),


            uiOutput(outputId = 'image')


        )
    )
)
