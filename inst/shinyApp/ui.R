
dashboardPage(

#### DashBoard Header ####
    dashboardHeader(
        title =  titlePanel(
            title = h2('ProteoViewer'),

            windowTitle = tags$head(
                tags$link(
                    rel = "icon",
                    type = "image/png",
                    href = "logo_small.png"
                    ),
                tags$title("ProteoViewer"),

                # Javascript scripts to zoom in/out

                tags$script(src = "https://unpkg.com/panzoom@9.4.0/dist/panzoom.min.js"),

                # Custom

                tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),


                )
            ),
        tags$li(a(id = 'power-off',
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
                    text = h3('Results'),

                    tabName = 'proteinViz',

                    selected = TRUE,

                    startExpanded = FALSE,

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

                    #uiOutput('downloadSingleUI')

                    uiOutput('downloadersUI'),

                    br()

                    ),

                tabName = 'proteinviz'
                ),

            ProteoViewer:::convertMenuItem(

                menuItem(
                    h3('Experiment Design'),
                    tabName = 'experimentDesignTab',

                    fileInput(inputId = "expDesignUpload",
                        label =  h4("Upload the experiment design file"),
                        multiple = FALSE,
                        accept = 'text')
                    ),
                tabName = 'expDesigntab'
                ),


            ProteoViewer:::convertMenuItem(

                menuItem(
                    h3('Demo Data'),
                    tabName = 'demoTab',

                    br(),

                    shinyWidgets::actionBttn(
                        inputId = 'Demo',
                        label = 'Load demo data',
                        icon = NULL,
                        style = "unite",
                        color = "default",
                        size = "md",
                        block = FALSE,
                        no_outline = TRUE)

                ),
                tabName = 'demoDataTab'
            )
            )


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
                           p(tags$em("Last updated: February 2022"),
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
                           p(tags$em("Last updated: February 2022"),
                             style = 'font-size:75%')
                           )
                    )
                ),


            tabItem(

                tabName = 'demoTab',

                fluidRow(

                    column(
                        width = 12,
                        h3("Proteomics Data"),


                        h4(
                            "ProteoViewer requires the evidence.txt file created by MaxQuant.
                      You can download an example of this file here: "
                        ),

                        tags$a(href='data/evidence.txt',
                               h4('evidence.txt'),
                               download=NA,
                               target='_blank'),

                        h3("Experiment design"),
                        h4("If you want to compare different conditions, you can do so
                      by editing the table in Experiment Design. Alternatively,
                      you can also upload an experiment design table, in .txt format.
                      You can download an example of the experiment_design.txt here:"
                        ),

                        tags$a(href='data/experiment_design_example.txt',
                               h4('experiment_design.txt'),
                               download=NA,
                               target='_blank'),
                        h4("By clicking on Load demo data, these two files will be uploaded automatically")
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
                           p(tags$em("Last updated: February 2022"),
                             style = 'font-size:75%')
                           )
                    )
                )
            )
        )
)
