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
          helpText(h3("Color")),
          selectInput(ns("color"),"Heatmap",c("magma","inferno","plasma","viridis",
                                       "cividis","rocket","mako","turbo"),selected="magma"),
          selectInput(ns("bg_color"),"Background",c("white","black"),selected="white"),
          helpText(h3("Legend")),
          textInput(ns("legend_name"),"Enter a legend name",value = "legendname"),
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

    fun_color <- reactive({
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
    })

    aff_color <- reactive({
      if(input$bg_color == "white"){
        return("black")
      }else{
        return("white")
      }
    })


    plot <- eventReactive(input$val_a1,{
      req(r$fil_df)
      if(input$Dend_row == "No" && input$Dend_col == "No"){
        mat <- r$fil_df()
        mat <- mat[seriation::get_order(r$HC_l()), seriation::get_order(r$HC_c())]
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
                                                 cluster_rows = FALSE,
                                                 cluster_columns = FALSE,
                                                 col = fun_color(),
                                                 column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                 row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
                                                                           col=aff_color()),
                                                 column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
                                                                              col=aff_color()),
                                                 heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        )
      }else if(input$Dend_row == "No" && input$Dend_col == "Yes"){
        mat <- r$fil_df()
        mat <- mat[seriation::get_order(r$HC_l()),]
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
                                                 cluster_rows = FALSE,
                                                 cluster_columns = stats::as.dendrogram(r$HC_c()),
                                                 column_dend_gp = gpar(col = aff_color()),
                                                 col = fun_color(),
                                                 column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                 row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
                                                                           col=aff_color()),
                                                 column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
                                                                              col=aff_color()),
                                                 heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        )
      }else if(input$Dend_row == "Yes" && input$Dend_col == "No"){
        mat <- r$fil_df()
        mat <- mat[, seriation::get_order(r$HC_c())]
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
                                                 cluster_rows = stats::as.dendrogram(r$HC_l()),
                                                 row_dend_gp = gpar(col = aff_color()),
                                                 cluster_columns = FALSE,
                                                 col = fun_color(),
                                                 column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                 row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
                                                                           col=aff_color()),
                                                 column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
                                                                              col=aff_color()),
                                                 heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        )
      }else{
        Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(r$fil_df()), name = input$legend_name,
                                               cluster_rows = stats::as.dendrogram(r$HC_l()),
                                               row_dend_gp = gpar(col = aff_color()),
                                               cluster_columns = stats::as.dendrogram(r$HC_c()),
                                               column_dend_gp = gpar(col = aff_color()),
                                               col = fun_color(),
                                               column_names_max_height = max_text_width(colnames(r$fil_df())),
                                               row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
                                                                         col=aff_color()),
                                               column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
                                                                            col=aff_color()),
                                               heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
                                              )
      }
      draw(Heatmapsimple, background = input$bg_color)
      })


    output$ht_simple <- renderPlot({
      req(plot)
      plot()
    })

    output$down <- downloadHandler(
      filename =  function() {
        paste(input$legend_name,"pdf",sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        grDevices::pdf(file) # open the pdf device
        # req(r$fil_df)
        # if(input$Dend_row == "No" && input$Dend_col == "No"){
        #   mat <- r$fil_df()
        #   mat <- mat[seriation::get_order(r$HC_l()), seriation::get_order(r$HC_c())]
        #   Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
        #                                            cluster_rows = FALSE,
        #                                            cluster_columns = FALSE,
        #                                            col = fun_color(),
        #                                            column_names_max_height = max_text_width(colnames(r$fil_df())),
        #                                            row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
        #                                                                      col=aff_color()),
        #                                            column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
        #                                                                         col=aff_color()),
        #                                            heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        #   )
        # }else if(input$Dend_row == "No" && input$Dend_col == "Yes"){
        #   mat <- r$fil_df()
        #   mat <- mat[seriation::get_order(r$HC_l()),]
        #   Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
        #                                            cluster_rows = FALSE,
        #                                            cluster_columns = stats::as.dendrogram(r$HC_c()),
        #                                            column_dend_gp = gpar(col = aff_color()),
        #                                            col = fun_color(),
        #                                            column_names_max_height = max_text_width(colnames(r$fil_df())),
        #                                            row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
        #                                                                      col=aff_color()),
        #                                            column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
        #                                                                         col=aff_color()),
        #                                            heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        #   )
        # }else if(input$Dend_row == "Yes" && input$Dend_col == "No"){
        #   mat <- r$fil_df()
        #   mat <- mat[, seriation::get_order(r$HC_c())]
        #   Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
        #                                            cluster_rows = stats::as.dendrogram(r$HC_l()),
        #                                            row_dend_gp = gpar(col = aff_color()),
        #                                            cluster_columns = FALSE,
        #                                            col = fun_color(),
        #                                            column_names_max_height = max_text_width(colnames(r$fil_df())),
        #                                            row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
        #                                                                      col=aff_color()),
        #                                            column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
        #                                                                         col=aff_color()),
        #                                            heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        #   )
        # }else{
        #   Heatmapsimple <- ComplexHeatmap::Heatmap(as.matrix(r$fil_df()), name = input$legend_name,
        #                                            cluster_rows = stats::as.dendrogram(r$HC_l()),
        #                                            row_dend_gp = gpar(col = aff_color()),
        #                                            cluster_columns = stats::as.dendrogram(r$HC_c()),
        #                                            column_dend_gp = gpar(col = aff_color()),
        #                                            col = fun_color(),
        #                                            column_names_max_height = max_text_width(colnames(r$fil_df())),
        #                                            row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
        #                                                                      col=aff_color()),
        #                                            column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
        #                                                                         col=aff_color()),
        #                                            heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        #   )
        # }
        # draw(Heatmapsimple, background = input$bg_color)# draw the plot
        grDevices::dev.off()  # turn the device off
      })


  })
}

## To be copied in the UI
# mod_heatmap_simple_ui("heatmap_simple_1")

## To be copied in the server
# mod_heatmap_simple_server("heatmap_simple_1")
