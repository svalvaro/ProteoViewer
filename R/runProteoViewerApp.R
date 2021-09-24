#' Title
#'
#' @return
#' @export
#'
#' @examples
runProteoViewerApp <- function(){
    shiny::runApp(appDir = base::system.file('shinyApp/',
                                       package = 'ProteoViewer'))
}
