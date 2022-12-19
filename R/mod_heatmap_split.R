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
#' @importFrom dynamicTreeCut cutreeHybrid
#' @importFrom circlize colorRamp2
#' @importFrom grDevices rainbow
#' @useDynLib biseriatedheatmaps, .registration = TRUE
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
          #Tabs pour afficher mode cluster ou point de transition
          parameter_tabs <- tabsetPanel(
            id = ns("params"),
            type = "hidden",
            tabPanel("cluster",
                     radioButtons(ns("meth_split"),"Methode of Split",
                                  choices = list("cutree","cutreeHybrid"),
                                  selected = "cutree",inline = TRUE),
                     #tabs pour cutree ou cutreeHybrid
                     parameter_tabs <- tabsetPanel(
                       id = ns("params_clus"),
                       type = "hidden",
                       tabPanel("cutree",
                                column(6,
                                  numericInput(ns("Krow"),"Number of row clusters",value=3,min=1)
                                ),
                                column(6,
                                  numericInput(ns("Kcol"),"Number of column clusters",value=4,min=1)
                                ),
                                radioButtons(ns("cutree_dend"), "Show dendrogram",
                                             choices = list("Yes", "No"),
                                             selected = "Yes",inline = TRUE
                                )
                       ),
                       tabPanel("cutreeHybrid",
                                column(6,
                                  helpText(h4("Row")),
                                  numericInput(ns("minsize_row"), "MinClusterSize", value = 1, min = 1),
                                  sliderInput(ns("ds_row"), "deepSplit", value = 0, min = 0, max = 4)
                                ),
                                column(6,
                                  helpText(h4("Column")),
                                  numericInput(ns("minsize_col"), "MinClusterSize", value = 1, min = 1),
                                  sliderInput(ns("ds_col"), "deepSplit", value = 0, min = 0, max = 4)
                                ),
                       )
                     ),
            ),
            tabPanel("ecp",
                     helpText(h3("En creation")),
            )
        ),







##ui for heatmap
          helpText(h3("Heatmap")),
          column(6,
            selectInput(ns("color"),"Heatmap color",c("magma","inferno","plasma","viridis",
                                              "cividis","rocket","mako","turbo"),selected="magma")
            ),
          column(6,
            selectInput(ns("bg_color"),"Background color",c("white","black"),selected="white")
            ),
          textInput(ns("legend_name"),"Enter a legend name",value = "legendname"),
          actionButton(ns("val_a2"), "valider"),
          width=12
      ),
      box(title = "Heatmap", status = "primary", solidHeader = TRUE, collapsible = FALSE,
          shinycssloaders::withSpinner(plotOutput(ns("ht_split"), height = "600px")),
          downloadButton(ns("down"), label = "Download the plot", style="color:#000000; display: block"),
          width=10
      ),

box(title = "clusters", status = "success", solidHeader = TRUE,
    shinycssloaders::withSpinner(verbatimTextOutput(ns("clust"))),
    downloadButton(ns("down_data"), label = "Download clusters", style="color:#000000; display: block"),
    width=12)



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
                                                   border=aff_color(),
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
                                                   border=aff_color(),
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
                                                   border=aff_color(),
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

###annotation cluster

    ####cluster ligne
    clus_r <- reactive({
      if(input$meth_split=="cutree"){
        cth <- stats::cutree(r$HC_l(),k=input$Krow)
      }else{
        cth <- dynamicTreeCut::cutreeHybrid(r$HC_l(), as.matrix(r$distm_ml()),
                                            minClusterSize = input$minsize_row, deepSplit = input$ds_row,
                                            verbose = 0)$labels
      }
      cth <- cth[get_order(r$HC_l())]
      cth <- cth_numgroup(cth)
    })

    anno_r <- reactive({
      VmA <- c(1:length(clus_r()))
      mrep <- c(length(clus_r()),0)
      vrep <- rep(mrep,length(clus_r()))
      vcutch <- clus_r()
      for(j in VmA){vcutch[which(vcutch==j)]<-j+vrep[j]}
      pal = circlize::colorRamp2(as.integer(levels(as.factor(vcutch))), grDevices::rainbow(n = nlevels(as.factor(vcutch))))
      RowAN <- rowAnnotation(Cluster = anno_simple(vcutch, na_col=input$bg_color, col = pal),
                             annotation_name_gp = gpar(col =aff_color()))
    })

    group_r <- reactive({
      names <- rownames(r$M_ser()) #on recupere les noms des lignes du data.frame
      gr <- list()
      for(i in unique(clus_r())){
        if(i!=0){
          gr[[i]] <- names[which(clus_r()==i)]
        }
      }
      return(gr)
    })

    ####cluster colonne
    clus_c <- reactive({
      if(input$meth_split=="cutree"){
        cth <- stats::cutree(r$HC_c(),k=input$Kcol)
      }else{
        cth <- dynamicTreeCut::cutreeHybrid(r$HC_c(), as.matrix(r$distm_mc()),
                                            minClusterSize = input$minsize_col, deepSplit = input$ds_col,
                                            verbose = 0)$labels
      }
      cth <- cth[get_order(r$HC_c())]
      cth <- cth_numgroup(cth)
    })

    anno_c <- reactive({
      VmA <- c(1:length(clus_c()))
      mrep <- c(length(clus_c()),0)
      vrep <- rep(mrep,length(clus_c()))
      vcutch <- clus_c()
      for(j in VmA){vcutch[which(vcutch==j)]<-j+vrep[j]}
      pal = circlize::colorRamp2(as.integer(levels(as.factor(vcutch))), grDevices::rainbow(n = nlevels(as.factor(vcutch))))
      ColumnAN <- HeatmapAnnotation(Cluster = anno_simple(vcutch, na_col=input$bg_color, col = pal),
                                    annotation_name_gp = gpar(col = aff_color()))
    })

    group_c <- reactive({
      names <- colnames(r$M_ser()) #on recupere les noms des colonnes du data.frame
      gr <- list()
      for(i in unique(clus_c())){
        if(i!=0){
          gr[[i]] <- names[which(clus_c()==i)]
        }
      }
      return(gr)
    })

    ####groupe matrice
    group_rc <- reactive({
      names_r <- rownames(r$M_ser()) #on recupere les noms des lignes du data.frame
      names_c <- colnames(r$M_ser()) #on recupere les noms des colonnes du data.frame
      gr <- list()
      for(i in unique(clus_r())){
        gr[[i]]<-list()
        for(j in unique(clus_c())){
          if(i!=0 && j!=0){
          gr[[i]][[j]] <- c(names_r[which(clus_r()==i)],names_c[which(clus_c()==j)])
          }
        }
      }
      return(gr)
    })

    ####affichage
    clusAnno <- reactive({
      if(input$split_row == "No" && input$split_col == "Yes"){
        mat <- r$M_ser()
        Heatmapsplit <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
                                                cluster_rows = FALSE,
                                                cluster_columns = FALSE,
                                                column_split = clus_c(),
                                                column_title = NULL,
                                                top_annotation = anno_c(),
                                                col = fun_color(),
                                                border=aff_color(),
                                                column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
                                                                          col=aff_color()),
                                                column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
                                                                             col=aff_color()),
                                                heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        )
      }else if(input$split_row == "Yes" && input$split_col == "No"){
        mat <- r$M_ser()
        Heatmapsplit <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
                                                cluster_rows = FALSE,
                                                row_split = clus_r(),
                                                row_title = NULL,
                                                left_annotation = anno_r(),
                                                cluster_columns = FALSE,
                                                col = fun_color(),
                                                border=aff_color(),
                                                column_names_max_height = max_text_width(colnames(r$fil_df())),
                                                row_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(nrow(r$fil_df())),
                                                                          col=aff_color()),
                                                column_names_gp = grid::gpar(fontsize = 0.2 + 1/log10(ncol(r$fil_df())),
                                                                             col=aff_color()),
                                                heatmap_legend_param = list(title_gp = gpar(col=aff_color()),labels_gp = gpar(col=aff_color())),
        )
      }else{
        mat <- r$M_ser()
        Heatmapsplit <- ComplexHeatmap::Heatmap(as.matrix(mat), name = input$legend_name,
                                                cluster_rows = FALSE,
                                                row_split = clus_r(),
                                                row_title = NULL,
                                                left_annotation = anno_r(),
                                                cluster_columns = FALSE,
                                                column_split = clus_c(),
                                                column_title = NULL,
                                                top_annotation = anno_c(),
                                                col = fun_color(),
                                                border=aff_color(),
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

    htsplot <- eventReactive(input$val_a2,{
      if(input$split_row == "No" && input$split_col == "No"){
        textplot("Choose to split on row or column,\n or go to Heatmap\n",cex=1.5)
      }
      else{
        if(input$typ_split=="cluster" && input$meth_split=="cutree" && input$cutree_dend=="Yes"){
          req(r$fil_df)
          return(cutreedendro())
        }
        else if(input$typ_split=="cluster" &&
                ((input$meth_split=="cutree" && input$cutree_dend=="No")||
                 (input$meth_split=="cutreeHybrid"))){
          req(r$M_ser)
          return(clusAnno())
        }
      }
    })

    group <- eventReactive(input$val_a2,{
      if(input$split_row == "No" && input$split_col == "No"){
        return(cat("Choose to split on row or column, or go to Heatmap"))
      }
      else{
        if(input$typ_split=="cluster" && input$meth_split=="cutree" && input$cutree_dend=="Yes"){
          req(r$fil_df)
          return(cat("To see name in group, choose no dendrogram"))
        }
        else if(input$typ_split=="cluster" &&
                ((input$meth_split=="cutree" && input$cutree_dend=="No")||
                 (input$meth_split=="cutreeHybrid"))){
          req(r$M_ser)
          if(input$split_row == "No" && input$split_col == "Yes"){
            return(group_c())
          }else if(input$split_row == "Yes" && input$split_col == "No"){
            return(group_r())
          }else{
            return(group_rc())
          }
        }
      }
    })

##output
    output$ht_split <- renderPlot({
      req(htsplot)
      htsplot()
    })

    output$clust <- renderPrint({
      group()
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

    output$down_data <- downloadHandler(
      filename =  function() {
        paste0("clusters_",input$legend_name,".txt")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        sink(file)
        print(group())
        sink()
      })


  })
}

## To be copied in the UI
# mod_heatmap_split_ui("heatmap_split_1")

## To be copied in the server
# mod_heatmap_split_server("heatmap_split_1")
