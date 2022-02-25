uiHeader <- dashboardHeader(
    title =  titlePanel(
        title = h2('ProteoViewer'),

        windowTitle = tags$head(
            tags$link(
                rel = "icon",
                type = "image/png",
                href = "images/logo_small.png"
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
        img(src = 'images/logo.png',
            title = "ProteoLab",
            height = "30px"),
        style = "padding-top:10px; padding-bottom:10px;"),
        class = "dropdown")
)
