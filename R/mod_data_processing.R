#' data_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import seriation
#' @import DendSer
#' @import stats
#' @importFrom shiny NS tagList
mod_data_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Data Processing",status = "primary",solidHeader = TRUE,
          helpText("choose your parameters"),
          helpText(h3("Data type")),
          radioButtons(ns("typ_data"),"",choices = c( #Ajouter des informations d'aides explications
            "Binary",
            "Numerical"),
            selected = "Binary",inline = TRUE),
#Ajout mise à jour pour afficher choix pour donner binaire ou donner numérique, voir mastering shiny 10.2.1 Conditional UI
#Tabs pour afficher mode binary ou mode numerical
          parameter_tabs <- tabsetPanel(
            id = ns("params"),
            type = "hidden",
            tabPanel("Binary",
                     helpText(h3("Filtering")),
                     helpText("Enter for rows and columns the minimum number of zeros (min0)
                              and the minimum number of ones (min1).
                              If you do not want to make any changes, enter zero."),
                     column(6,
                            helpText(h5("Line")),
                            numericInput(ns("fl_max"), "min0", value = 1),
                            numericInput(ns("fl_min"), "min1", value = 2)
                     ),
                     column(6,
                            helpText(h5("Column")),
                            numericInput(ns("fc_max"), "min0", value = 0),
                            numericInput(ns("fc_min"), "min1", value = 0)
                     ),
                     helpText(h3("Distance calculation")),
                     helpText("See method of ade4::dist.binary"),
                     selectInput(ns('inDist'),"Distance",
                                 c("S2 coefficient of Gower & Legendre", "S3 Jaccard index","S4 Simple matching coefficient of Sokal & Michener",
                                  "S5 Sokal & Sneath","S6 Rogers & Tanimoto",
                                  "S7 Dice or Sorensen","S9 Hamann coefficient",
                                  "S12 Ochiai", "S13 Sokal & Sneath", "S14 Phi of Pearson"),
                                 selected = "S12 Ochiai")
            ),
            tabPanel("Numerical",
                     helpText(h3("Distance calculation")),
                     selectInput(ns('inDist_num'),"Distance", c("euclidean","maximum",
                                                            "manhattan","canberra",
                                                            "binary","minkowski"), selected = "binary"),
            )
          ),
        helpText(h3("Hierarchical clustering")),
        selectInput(ns('inHC'),"Clustering hierarchique", c("ward.D","ward.D2",
                                                            "single","complete",
                                                            "average","mcquitty",
                                                            "median","centroid","diana"),
                    selected = "diana"),
        helpText(h3("Seriation")),
        radioButtons(ns('ser'),"Seriation", choices = c("Oui","Non"), selected="Oui", inline = TRUE),
        width = 12
      )
    )
  )
}

#' data_processing Server Functions
#'
#' @noRd
mod_data_processing_server <- function(id, r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$typ_data, {
      updateTabsetPanel(inputId = "params", selected = input$typ_data)
    })

    #mettre les donnees apres filtrage, filtrage en fonction bianire ou non
    r$fil_df <- reactive({
      req(r$df)
      datamat <- r$df()
      if(input$typ_data == "Binary"){
          datamat <- datamat[rowSums(datamat)>=input$fl_min,]
          datamat <- datamat[rowSums(datamat)<=(ncol(datamat)-input$fl_max),]
          datamat <- datamat[,colSums(datamat)>=input$fc_min]
          datamat <- datamat[,colSums(datamat)<=(nrow(datamat)-input$fc_max)]
      }else{
        #ajouter ici filtre pour numerique
      }
      return(datamat)
    })

    meth_dist <- reactive({
      switch(input$inDist,
             "S2 coefficient of Gower & Legendre" = 10,
             "S3 Jaccard index" = 1,
             "S4 Simple matching coefficient of Sokal & Michener" = 2,
             "S5 Sokal & Sneath" = 3,
             "S6 Rogers & Tanimoto" = 4,
             "S7 Dice or Sorensen" = 5,
             "S9 Hamann coefficient" = 6,
             "S12 Ochiai" = 7,
             "S13 Sokal & Sneath" = 8,
             "S14 Phi of Pearson" = 9
      )
    })

    #distance ligne
    r$distm_ml <- reactive({
      if(input$typ_data == "Binary"){
        dMat <- ade4::dist.binary(r$fil_df(), method = meth_dist(), diag = FALSE, upper = FALSE)
      }else{
         dMat <- stats::dist(r$fil_df(), method = input$inDist_num)
      }
    })

    #distance colonne
    r$distm_mc <- reactive({
      if(input$typ_data == "Binary"){
        TdMat <- ade4::dist.binary(t(as.matrix(r$fil_df())), method = meth_dist(), diag = FALSE, upper = FALSE)
      }else{
        TdMat <- stats::dist(t(as.matrix(r$fil_df())), method = input$inDist_num)
      }
    })

    r$HC_l <- reactive({
      if(input$inHC != "diana"){
        HC <- stats::hclust(r$distm_ml(), method= input$inHC)
      } else{
        HC <- stats::as.hclust(cluster::diana(r$distm_ml())) #HC avec diana du package cluster
      }
      if(input$ser=="Oui"){
        OrdSer <- DendSer::DendSer(HC, r$distm_ml(), cost= costARc) #calcul de la seriation avec DendSer du package DendSer
        HC <-  seriation::permute(HC, OrdSer)
      }
      return(HC)
    })

    r$HC_c <- reactive({
      if(input$inHC != "diana"){
        HC <- stats::hclust(r$distm_mc(), method= input$inHC)
      } else{
        HC <- stats::as.hclust(cluster::diana(r$distm_mc())) #HC avec diana du package cluster
      }
      if(input$ser=="Oui"){
        OrdSer <- DendSer::DendSer(HC, r$distm_mc(), cost= costARc) #calcul de la seriation avec DendSer du package DendSer
        HC <-  seriation::permute(HC, OrdSer)
      }
      return(HC)
    })

    #matrice ordonnee
    r$M_ser <- reactive({
      rser <- r$fil_df()
      rser <- rser[seriation::get_order(r$HC_l()), seriation::get_order(r$HC_c())]
    })

    r$typ_data <- reactive({td <- input$typ_data})

  })
}

## To be copied in the UI
# mod_data_processing_ui("data_processing_1")

## To be copied in the server
# mod_data_processing_server("data_processing_1")
