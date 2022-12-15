#' heatmap_simple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shinycssloaders
#' @import ComplexHeatmap
#' @import grid
#' @import stats
#' @import seriation
#' @importFrom shiny NS tagList
mod_heatmap_simple_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Parameter", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          helpText(h3("Show Dendrogram")),
          column(3,
                 radioButtons(ns("Dend_row"), "Row",
                    choices = list("Yes", "No"),
                    selected = "Yes",inline = TRUE
                 )
          ),
          column(3,
                 radioButtons(ns("Dend_col"), "Column",
                              choices = list("Yes", "No"),
                              selected = "Yes",inline = TRUE
                 )
          ),
          actionButton(ns("val_a1"), "valider"),
          width=12
      ),
      box(title = "Heatmap", status = "primary", solidHeader = TRUE, collapsible = FALSE,
          shinycssloaders::withSpinner(plotOutput(ns("ht_simple"), height = "600px")),
           width=10
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


    plot <- eventReactive(input$val_a1,{
      req(r$fil_df)
      if(input$Dend_row == "No" && input$Dend_col == "No"){
        mat <- r$fil_df()
        mat <- mat[seriation::get_order(r$HC_l()), seriation::get_order(r$HC_c())]
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = "Heatmapsimple",
                                                 cluster_rows = FALSE, cluster_columns = FALSE,
                                                 col = viridis::magma(256), column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                 row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df()))),
                                                 column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df()))),
        )
      }else if(input$Dend_row == "No" && input$Dend_col == "Yes"){
        mat <- r$fil_df()
        mat <- mat[seriation::get_order(r$HC_l()),]
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = "Heatmapsimple",
                                                 cluster_rows = FALSE, cluster_columns = stats::as.dendrogram(r$HC_c()),
                                                 col = viridis::magma(256), column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                 row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df()))),
                                                 column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df()))),
        )
      }else if(input$Dend_row == "Yes" && input$Dend_col == "No"){
        mat <- r$fil_df()
        mat <- mat[, seriation::get_order(r$HC_c())]
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = "Heatmapsimple",
                                                 cluster_rows = stats::as.dendrogram(r$HC_l()), cluster_columns = FALSE,
                                                 col = viridis::magma(256), column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                 row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df()))),
                                                 column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df()))),
        )
      }else{
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(r$fil_df()), name = "Heatmapsimple",
                                               cluster_rows = stats::as.dendrogram(r$HC_l()), cluster_columns = stats::as.dendrogram(r$HC_c()),
                                               col = viridis::magma(256), column_names_max_height = max_text_width(colnames(r$fil_df())),
                                               row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df()))),
                                               column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df()))),
                                              )
      }
      draw(Heatmapsimple)
      })


    output$ht_simple <- renderPlot({
      req(plot)
      plot()
    })


  })
}

## To be copied in the UI
# mod_heatmap_simple_ui("heatmap_simple_1")

## To be copied in the server
# mod_heatmap_simple_server("heatmap_simple_1")
