#' heatmap_split UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_heatmap_split_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' heatmap_split Server Functions
#'
#' @noRd
mod_heatmap_split_server <- function(id, r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_heatmap_split_ui("heatmap_split_1")

## To be copied in the server
# mod_heatmap_split_server("heatmap_split_1")
