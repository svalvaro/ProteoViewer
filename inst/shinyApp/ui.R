dashboardPage(

#### DashBoard Header ####
    dashboardHeader(
        title =  titlePanel(
            title = 'ProteoViewer',

            windowTitle = tags$head(
                tags$link(
                    rel = "icon",
                    type = "image/png",
                    href = "logo_small.png"
                    ),
                tags$title("ProteoViewer")
                )
            ),
        tags$li(a(
            href = 'https://fgu.cas.cz/departments/proteomicka-servisni-laborator',
            icon("power-off"),
            title = "Close ProteoViewer"),
            class = "dropdown"),

        tags$li(a(
            href = 'https://fgu.cas.cz/departments/proteomicka-servisni-laborator',
            img(src = 'logo.png',
                title = "ProteoLab",
                height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown")
        ),

#### DashBoard side bar ####
    dashboardSidebar(
        width = 300,

        sidebarMenu(width = 300,
            id = 'tabs_selected',

            ProteoViewer:::convertMenuItem(
                menuItem(
                    text = 'Protein Visualization',

                    tabName = 'proteinViz',

                    selected = TRUE,

                    startExpanded = TRUE,

                    fileInput(
                        inputId = "proteomics_table",
                        label =  h4("Upload the evidence.txt file"),
                        multiple = FALSE,
                        accept = 'text'
                    ),

                    # Select the protein of interest
                    uiOutput('proteinsSelect'),

                    uiOutput('comparisonAll'),

                    # Compare between conditions based on experiment design
                    #uiOutput('comparisonCheck'),

                    uiOutput('comparisonSelector'),

                    br(),

                    uiOutput('combineExperimentsOutput'),

                    uiOutput('experimentSelect'),

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

                        selected = 'none'),

                    sliderInput(
                        inputId = 'zoomFigure',
                        label = h4('Select the size of the image'),
                        min = 20, max = 2000, value = 150)
                    ),

                tabName = 'proteinviz'
                ),

            ProteoViewer:::convertMenuItem(

                menuItem(
                    'Experiment Design',
                    tabName = 'experimentDesignTab',

                    fileInput(inputId = "expDesignUpload",
                        label =  h4("Upload the experiment design file"),
                        multiple = FALSE,
                        accept = 'text')
                    ),
                tabName = 'expDesigntab'
                )
            ),

        shinyWidgets::actionBttn(
            inputId = 'Demo',
            label = 'Start Demo',
            icon = NULL,
            style = "unite",
            color = "default",
            size = "md",
            block = FALSE,
            no_outline = TRUE)
        ),

#### DashBoard Body ####
    dashboardBody(
        tabItems(
            tabItem(
                tabName = 'proteinViz',

                fluidRow(style = "height:5000px;",

                    column(
                        width = 8,



                            uiOutput('UserInterNoGroups'),

                            uiOutput('UserInterGroups'),

                            plotOutput('legend')

                        ),
                    column(
                        width = 4,
                        box(plotOutput('legendPTMs')
                            )
                        )
                    )
                ),

            tabItem(
                tabName = 'experimentDesignTab',

                fluidRow(
                    box(title = 'Experiment Design',

                        rhandsontable::rHandsontableOutput(
                            'experimentDesignOutput')
                        )
                    )
                )
            )
        )
)
