uiSideBar <- dashboardSidebar(
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
                            label = h3('Load demo data'),
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
)
