#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues()
  mod_data_loading_server("data_loading_1",r=r) #Dans r r$df
  mod_data_processing_server("data_processing_1", r=r)# dans r r$fil_df,  r$M_ser,  r$distm_ml,  r$distm_mc,  r$HC_l,  r$HC_c
  mod_heatmap_simple_server("heatmap_simple_1", r=r)#
  mod_heatmap_split_server("heatmap_split_1", r=r)
  mod_heatmap_analysis_server("heatmap_analysis_1",r=r)
  mod_information_server("information_1")
}
