
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
                tags$title("ProteoViewer"),

                # Javascript scripts to zoom in/out

                tags$script(src = "javascript/zoom1.js"),
                tags$script(src = "javascript/zoom2.js"),
                tags$script(src = "javascript/zoom3.js"),
                tags$script(src = "https://unpkg.com/panzoom@9.4.0/dist/panzoom.min.js"),




                # Custom
                shiny::includeCSS(path = "www/css/styles.css")
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

                    # Select the type of peptide to visualize

                    uiOutput('peptidesIntensitySelector'),

                    # Select the protein of interest
                    uiOutput('proteinsSelect'),

                    uiOutput('comparisonAll'),

                    # Compare between conditions based on experiment design
                    #uiOutput('comparisonCheck'),

                    uiOutput('comparisonSelector'),

                    br(),

                    uiOutput('combineExperimentsOutput'),

                    uiOutput('experimentSelect'),


                    uiOutput('proteaseSelector'),

                    uiOutput('imageSizeSelector')


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

                # First plot the legends independently of the selection
                # Since it's the same for all proteins
                uiOutput('legendIntensities'),

                fluidRow(
                    # Plotting of the Rendered proteins
                    column(width = 8,

                        uiOutput('proteinImagesUIAll')
                        #imageOutput('proteinImageNoComparison')
                        ),

                    # Plotting of the PTM image and the table
                    column(width = 4,
                        uiOutput('PTMSlegendUI')
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
