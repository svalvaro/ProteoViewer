dashboardPage(

    dashboardHeader(#title = "ProteoViewer",
        title = titlePanel('ProteoViewer',
                           tags$head(
                               tags$link(
                                   rel = "icon", type = "image/png", href = "logo.png"
                               ),
                               tags$title("ProteoViewer")#,

                               #tag$link(rel = 'icons', href = "Proteomika_logo_hires.png")


                           )
        ),
        titleWidth =  300),

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


        # checkboxInput('protease', h4('Show trypsin cutting sites'),
        #               value = TRUE),

        selectInput(inputId = 'proteaseSelected',
                    label =  'Protease',
                    choices = c('No protease' = 'none',
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
                    selected = 'none'),

        sliderInput(inputId = 'zoomFigure',
                    label = h4('Select the size of the image'),
                    min = 100, max = 5000, value = 1000),


        # Load demo data.
        shinyalert::useShinyalert(),

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
                h3(textOutput('error_message')),
                uiOutput(outputId = 'proteinImage'),

                plotOutput('lenged')
                )
            )
        )
    )
