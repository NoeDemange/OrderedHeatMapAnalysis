#' heatmap_split UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shinycssloaders
#' @import ComplexHeatmap
#' @importFrom gplots textplot
#' @importFrom dendextend color_branches
#' @importFrom shiny NS tagList
mod_heatmap_split_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Settings", status = "primary", solidHeader = TRUE, collapsible = TRUE,
##ui for split
          helpText(h3("Split")),
          column(3,
                 radioButtons(ns("split_row"), "Row",
                              choices = list("Yes", "No"),
                              selected = "Yes",inline = TRUE
                 )
          ),
          column(9,
                 radioButtons(ns("split_col"), "Column",
                              choices = list("Yes", "No"),
                              selected = "Yes",inline = TRUE
                 )
          ),
          radioButtons(ns("typ_split"),"Type of Split",
                       choiceNames = list("By cluster","By multiple change-point"),
                       choiceValues = list("cluster", "ecp"),
                       selected = "cluster",inline = TRUE),
          #Ajout mise à jour pour afficher choix pour donner binaire ou donner numérique, voir mastering shiny 10.2.1 Conditional UI
          #Tabs pour afficher mode binary ou mode numerical
          parameter_tabs <- tabsetPanel(
            id = ns("params"),
            type = "hidden",
            tabPanel("cluster",
                     radioButtons(ns("meth_split"),"Methode of Split",
                                  choices = list("cutree","cutreeHybrid"),
                                  selected = "cutree",inline = TRUE),
                     parameter_tabs <- tabsetPanel(
                       id = ns("params_clus"),
                       type = "hidden",
                       tabPanel("cutree",
                                column(6,
                                  numericInput(ns("Krow"),"Number of row clusters",value=3,min=1)
                                ),
                                column(6,
                                  numericInput(ns("Kcol"),"Number of column clusters",value=4,min=1)
                                )
                       ),
                       tabPanel("cutreeHybrid",
                                helpText("cutreeHybrid settings"),
                                numericInput(ns("minsize"), "MinClusterSize", value = 1, min = 1),
                                sliderInput(ns("ds"), "deepSplit", value = 0, min = 0, max = 4),
                       )
                     ),
            ),
            tabPanel("ecp",
                     helpText(h3("En creation")),
            )
        ),







##ui for heatmap
          helpText(h3("Heatmap")),
          selectInput(ns("color"),"Heatmap color",c("magma","inferno","plasma","viridis",
                                              "cividis","rocket","mako","turbo"),selected="magma"),
          selectInput(ns("bg_color"),"Background color",c("white","black"),selected="white"),
          textInput(ns("legend_name"),"Enter a legend name",value = "legendname"),
          actionButton(ns("val_a2"), "valider"),
          width=12
      ),
      box(title = "Heatmap", status = "primary", solidHeader = TRUE, collapsible = FALSE,
          shinycssloaders::withSpinner(plotOutput(ns("ht_split"), height = "600px")),
          downloadButton(ns("down"), label = "Download the plot", style="color:#000000; display: block"),
          width=10
      )
    )
  )
}

#' heatmap_split Server Functions
#'
#' @noRd
mod_heatmap_split_server <- function(id, r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
##update UI
    observeEvent(input$typ_split, {
      updateTabsetPanel(inputId = "params", selected = input$typ_split)
    })

    observeEvent(input$meth_split, {
      updateTabsetPanel(inputId = "params_clus", selected = input$meth_split)
    })

##color
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

##server
  ###cutree Dendro
    cutreedendro <- reactive({
         if(input$split_row == "No" && input$split_col == "Yes"){
          mat <- r$fil_df()
          mat <- mat[seriation::get_order(r$HC_l()),]
          dend <- stats::as.dendrogram(r$HC_c())
          dend <- color_branches(dend, k = input$Kcol)
          Heatmapsplit <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
                                                   cluster_rows = FALSE,
                                                   cluster_columns = dend,
                                                   column_split = input$Kcol,
                                                   column_title = NULL,
                                                   col = fun_color(),
                                                   column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                   row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
                                                                             col=aff_color()),
                                                   column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
                                                                                col=aff_color()),
                                                   heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
          )
        }else if(input$split_row == "Yes" && input$split_col == "No"){
          mat <- r$fil_df()
          mat <- mat[, seriation::get_order(r$HC_c())]
          dend <- stats::as.dendrogram(r$HC_l())
          dend <- color_branches(dend, k = input$Krow)
          Heatmapsplit <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
                                                   cluster_rows = dend,
                                                   row_split = input$Krow,
                                                   row_title = NULL,
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
          dend_row <- stats::as.dendrogram(r$HC_l())
          dend_row <- color_branches(dend_row, k = input$Krow)
          dend_col <- stats::as.dendrogram(r$HC_c())
          dend_col <- color_branches(dend_col, k = input$Kcol)
          Heatmapsplit <- ComplexHeatmap::Heatmap(as.matrix(r$fil_df()), name = input$legend_name,
                                                   cluster_rows = dend_row,
                                                   row_split = input$Krow,
                                                   row_title = NULL,
                                                   cluster_columns = dend_col,
                                                   column_split = input$Kcol,
                                                   column_title = NULL,
                                                   col = fun_color(),
                                                   column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                   row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
                                                                             col=aff_color()),
                                                   column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
                                                                                col=aff_color()),
                                                   heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
          )
        }
      draw(Heatmapsplit, background = input$bg_color)
    })

    htsplot <- htsplot <- eventReactive(input$val_a2,{
      if(input$split_row == "No" && input$split_col == "No"){
        textplot("Choose to split on row or column,\n or go to Heatmap\n",cex=1.5)
      }
      else{
        req(r$fil_df)
        if(input$typ_split=="cluster" && input$meth_split=="cutree"){
          return(cutreedendro())
        }
      }
    })

##output
    output$ht_split <- renderPlot({
      req(htsplot)
      htsplot()
    })

    output$down <- downloadHandler(
      filename =  function() {
        paste(input$legend_name,"pdf",sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        grDevices::pdf(file) # open the pdf device
        plot(htsplot())
        grDevices::dev.off()  # turn the device off
      })


  })
}

## To be copied in the UI
# mod_heatmap_split_ui("heatmap_split_1")

## To be copied in the server
# mod_heatmap_split_server("heatmap_split_1")
