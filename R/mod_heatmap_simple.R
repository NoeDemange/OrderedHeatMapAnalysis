#' heatmap_simple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shinycustomloader
#' @import ComplexHeatmap
#' @import grid
#' @import stats
#' @importFrom shiny NS tagList
mod_heatmap_simple_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Heatmap", status = "primary", solidHeader = TRUE, collapsible = FALSE,
          withLoader(plotOutput(ns("ht_simple"), height = "600px")),
          width=12
      )
    )
  )
}

#' heatmap_simple Server Functions
#'
#' @noRd
mod_heatmap_simple_server <- function(id, r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ht_simple <- renderPlot({
      req(r$fil_df)
      Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(r$fil_df()), name = "Heatmapsimple",
                               cluster_rows = stats::as.dendrogram(r$HC_l()), cluster_columns = stats::as.dendrogram(r$HC_c()),
                               col = viridis::magma(256), column_names_max_height = max_text_width(colnames(r$fil_df())),
                               row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df()))),
                               column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df()))),
                               )
      draw(Heatmapsimple)
    })


  })
}

## To be copied in the UI
# mod_heatmap_simple_ui("heatmap_simple_1")

## To be copied in the server
# mod_heatmap_simple_server("heatmap_simple_1")
