#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import InteractiveComplexHeatmap
#' @noRd
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues()
  mod_introduction_server("introduction_1")
  mod_data_loading_server("data_loading_1",r=r) #Dans r r$df
  mod_data_processing_server("data_processing_1", r=r)# dans r r$fil_df,  r$M_ser,  r$distm_ml,  r$distm_mc, r$HCNotPer_l, r$HC_l, r$HCNotPer_c, r$HC_c
  mod_heatmap_simple_server("heatmap_simple_1", r=r)# r$htplot
  mod_heatmap_split_server("heatmap_split_1", r=r) # r$htsplot
  mod_heatmap_analysis_server("heatmap_analysis_1",r=r) # r$htaplot
  mod_information_server("information_1")

  output$inf_white <- renderText({paste("Background should be set to 'white' in the",input$interHT_typ,"section for better visualization of labels."
  )})

  htplot_print <- eventReactive(input$val_interht,{
    if(input$interHT_typ == "Heatmap Ordering"){
      req(r$htplot)
      return(r$htplot())
    }
    else if(input$interHT_typ == "Heatmap Partitioning"){
      req(r$htsplot)
      return(r$htsplot())
    }
    else if(input$interHT_typ == "Heatmap Ordering Index Analysis"){
      req(r$htaplot)
      return(r$htaplot())
    }
  })

  observeEvent(htplot_print(),{
    makeInteractiveComplexHeatmap(input, output, session, htplot_print(), "HT_interactive")
  })

}
