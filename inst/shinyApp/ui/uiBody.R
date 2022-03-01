uiBody <-     dashboardBody(
    tabItems(
        tabItem(
            tabName = 'proteinViz',

            # First plot the legends independently of the selection
            # Since it's the same for all proteins
            uiOutput('legendIntensities'),

            fluidRow(
                # Plotting of the Rendered proteins
                column(width = 12,

                       uiOutput('proteinImagesUIAll')
                )
            ),

            fluidRow(
                column(width = 12,
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br()
                )
            ),

            fluidRow(
                column(width = 12,
                       uiOutput('coverageUI')
                )
            ),

            fluidRow(
                # Plotting of the PTM image and the table
                column(width = 12,
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
                       p(tags$em("Last updated: March 2022"),
                         style = 'font-size:75%')
                )
            )
        ),

        tabItem(
            tabName = 'experimentDesignTab',

            fluidRow(h1("Fill the Experiment Design")),

            fluidRow(

                column(width = 3,

                       actionBttn("saveExpDesign", "Update")
                ),
                column(width = 10,

                       box(
                           rhandsontable::rHandsontableOutput(
                               'experimentDesignOutput')
                       )
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
                       p(tags$em("Last updated: March 2022"),
                         style = 'font-size:75%')
                )
            )
        ),

        tabItem(

            tabName = 'demoTab',

            fluidRow(

                column(
                    width = 12,

                    h3('What is ProteoViewer'),

                    h4('ProteoViewer is a web application that takes proteomics
                       data and presents visualisations for the Protein Topology,
                       Protein Coverage, Peptide Intensity, and Post-translational Modifications.'),


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
                    h4("By clicking on Load demo data, these two files will be uploaded automatically"),


                    h3('Post-translational Modifications'),

                    h4('There are six PTMs which are recognised and colour-coded by ProteoViewer: Oxidation (M),
                        Acetyl (Protein N-term), Methyl (KR), Dimethyl (KR), Trimethyl (K) and Phospho (STY).
                        Other PTMs will be shown in the legend as "Other PTM".')
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
                       p(tags$em("Last updated: March 2022"),
                         style = 'font-size:75%')
                )
            )
        )
    )
)
