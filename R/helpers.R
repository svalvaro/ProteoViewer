#https://stackoverflow.com/questions/31794702/r-shiny-dashboard-tabitems-not-apparing
#' Title
#'
#' @param mi
#' @param tabName
#'
#' @return
#'
#' @examples
convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    mi
}
