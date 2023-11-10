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
#' @import viridis
#' @import dendextend
#' @importFrom shiny NS tagList
mod_heatmap_simple_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Settings", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          helpText(h3("Show Dendrogram")),
          column(3,
                 radioButtons(ns("Dend_row"), "Row",
                    choices = list("Yes", "No"),
                    selected = "Yes",inline = TRUE
                 )
          ),
          column(9,
                 radioButtons(ns("Dend_col"), "Column",
                              choices = list("Yes", "No"),
                              selected = "Yes",inline = TRUE
                 )
          ),
          helpText(h3("Heatmap")),
          parameter_tabs <- tabsetPanel(
            id = ns("params"),
            type = "hidden",
            tabPanel("Numerical",
                     selectInput(ns("color"),"Heatmap color",c("magma","inferno","plasma","viridis",
                                       "cividis","rocket","mako","turbo"),selected="turbo"),
                     ),
            tabPanel("Binary",)
            ),
          selectInput(ns("bg_color"),"Background color",c("white","black"),selected="black"),
          textInput(ns("legend_name"),"Enter a legend name",value = "Heatmap"),
          column(6,
                 numericInput(ns("heatmap_fontsize_col"), "Column fontsize", value = 4, min = 0, max = 100, step = 0.1),
          ),
          column(6,
                 numericInput(ns("heatmap_fontsize_row"), "Row fontsize", value = 4, min = 0, max = 100, step = 0.1),
          ),
          actionButton(ns("val_a1"), "valider"),
          width=12
      ),
      box(title = "Heatmap", status = "primary", solidHeader = TRUE, collapsible = FALSE,
          shinycssloaders::withSpinner(plotOutput(ns("ht_simple"), height = "600px")),
          downloadButton(ns("down"), label = "Download the plot", style="color:#000000; display: block"),
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
    ##update UI
    observeEvent(r$typ_data(),{
      updateTabsetPanel(inputId = "params", selected = r$typ_data())
    })

    fun_color <- reactive({
      if(r$typ_data() == "Binary"){
        col <- c("0"= "gray4", "1" = "lightgoldenrod3")
      }else{
        switch(input$color,
               "magma" = viridis::magma(256),
               "inferno" = viridis::inferno(256),
               "plasma" = viridis::plasma(256),
               "viridis" = viridis::viridis(256),
               "cividis" = viridis::cividis(256),
               "rocket" = viridis::rocket(256),
               "mako" = viridis::mako(256),
               "turbo" = viridis::turbo(256)
        )
      }
    })

    aff_color <- reactive({
      if(input$bg_color == "white"){
        return("black")
      }else{
        return("white")
      }
    })


    r$htplot <- eventReactive(input$val_a1,{
      req(r$fil_df)
      if(input$Dend_row == "No" && input$Dend_col == "No"){
        mat <- r$fil_df()
        mat <- mat[seriation::get_order(r$HC_l()), seriation::get_order(r$HC_c())]
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
                                                 cluster_rows = FALSE,
                                                 cluster_columns = FALSE,
                                                 col = fun_color(),
                                                 column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                 row_names_gp = grid::gpar(fontsize = input$heatmap_fontsize_row,
                                                                           col=aff_color()),
                                                 column_names_gp = grid::gpar(fontsize = input$heatmap_fontsize_col,
                                                                              col=aff_color()),
                                                 heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        )
      }else if(input$Dend_row == "No" && input$Dend_col == "Yes"){
        mat <- r$fil_df()
        mat <- mat[seriation::get_order(r$HC_l()),]
        dend <- stats::as.dendrogram(r$HCNotPer_c())
        dendextend::order.dendrogram(dend) <- seriation::get_order(r$HC_c())
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
                                                 cluster_rows = FALSE,
                                                 cluster_columns = dend,
                                                 column_dend_gp = gpar(col = aff_color()),
                                                 col = fun_color(),
                                                 column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                 row_names_gp = grid::gpar(fontsize = input$heatmap_fontsize_row,
                                                                           col=aff_color()),
                                                 column_names_gp = grid::gpar(fontsize = input$heatmap_fontsize_col,
                                                                              col=aff_color()),
                                                 heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        )
      }else if(input$Dend_row == "Yes" && input$Dend_col == "No"){
        mat <- r$fil_df()
        mat <- mat[, seriation::get_order(r$HC_c())]
        dend <- stats::as.dendrogram(r$HCNotPer_l())
        dendextend::order.dendrogram(dend) <- seriation::get_order(r$HC_l())
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
                                                 cluster_rows = dend,
                                                 row_dend_gp = gpar(col = aff_color()),
                                                 cluster_columns = FALSE,
                                                 col = fun_color(),
                                                 column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                 row_names_gp = grid::gpar(fontsize = input$heatmap_fontsize_row,
                                                                           col=aff_color()),
                                                 column_names_gp = grid::gpar(fontsize = input$heatmap_fontsize_col,
                                                                              col=aff_color()),
                                                 heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        )
      }else{
        dend_l <- stats::as.dendrogram(r$HCNotPer_l())
        dendextend::order.dendrogram(dend_l) <- seriation::get_order(r$HC_l())
        dend_c <- stats::as.dendrogram(r$HCNotPer_c())
        dendextend::order.dendrogram(dend_c) <- seriation::get_order(r$HC_c())
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(r$fil_df()), name = input$legend_name,
                                               cluster_rows = dend_l,
                                               row_dend_gp = gpar(col = aff_color()),
                                               cluster_columns = dend_c,
                                               column_dend_gp = gpar(col = aff_color()),
                                               col = fun_color(),
                                               column_names_max_height = max_text_width(colnames(r$fil_df())),
                                               row_names_gp = grid::gpar(fontsize = input$heatmap_fontsize_row,
                                                                         col=aff_color()),
                                               column_names_gp = grid::gpar(fontsize = input$heatmap_fontsize_col,
                                                                            col=aff_color()),
                                               heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
                                              )
      }
      draw(Heatmapsimple, background = input$bg_color)
      })


    output$ht_simple <- renderPlot({
      req(r$htplot)
      r$htplot()
    })

    output$down <- downloadHandler(
      filename =  function() {
        paste(input$legend_name,"pdf",sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        grDevices::pdf(file) # open the pdf device
        plot(r$htplot())
        grDevices::dev.off()  # turn the device off
      })


  })
}

## To be copied in the UI
# mod_heatmap_simple_ui("heatmap_simple_1")

## To be copied in the server
# mod_heatmap_simple_server("heatmap_simple_1")
