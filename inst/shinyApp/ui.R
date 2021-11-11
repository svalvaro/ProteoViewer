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
            href = 'https://proteomics.fgu.cas.cz/',
            icon("power-off"),
            title = "Close ProteoViewer"),
            class = "dropdown"),

        tags$li(a(
            href = 'https://proteomics.fgu.cas.cz/',
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

                fluidRow(
                    uiOutput('proteinImagesUIAll'),

                    uiOutput('PTMSlegendUI')
                    ),
                hr(style = "border-color: #cbcbcb;"),

                fluidRow(
                    column(9,

                           p("App created by ",
                             tags$a(href = "https://www.linkedin.com/in/svalvaro/",
                                    'Alvaro Sanchez-Villalba',
                                    target = '_blank'),
                             HTML("&bull;"),
                             style = "font-size: 85%"),

                           p("Have a question? Spot an error? Send an email ",
                             tags$a(href = "mailto:alvaro.sanchez@fgu.cas.cz",
                                    tags$i(class = 'fa fa-envelope',
                                           style = 'color:#990000'),
                                    target = '_blank'),
                             style = "font-size: 85%"),
                           p(tags$em("Last updated: November 2021"),
                             style = 'font-size:75%')
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
                    ),
                hr(style = "border-color: #cbcbcb;"),

                fluidRow(
                    column(9,

                           p("App created by ",
                             tags$a(href = "https://www.linkedin.com/in/svalvaro/",
                                    'Alvaro Sanchez-Villalba',
                                    target = '_blank'),
                             HTML("&bull;"),
                             style = "font-size: 85%"),

                           p("Have a question? Spot an error? Send an email ",
                             tags$a(href = "mailto:alvaro.sanchez@fgu.cas.cz",
                                    tags$i(class = 'fa fa-envelope',
                                           style = 'color:#990000'),
                                    target = '_blank'),
                             style = "font-size: 85%"),
                           p(tags$em("Last updated: October 2021"),
                             style = 'font-size:75%')
                           )
                    )
                )
            )
        )
)
