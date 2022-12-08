#' data_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Data Processing",status = "primary",solidHeader = TRUE,
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
                     helpText("Enter for rows and columns the maximum number of ones (max)
                              and the minimum number of ones (min).
                              If you do not want to make any changes, enter zero."),
                     column(6,
                            helpText(h5("Line")),
                            numericInput(ns("fl_min"), "min", value = 0),
                            numericInput(ns("fl_max"), "max", value = 0)
                     ),
                     column(6,
                            helpText(h5("Column")),
                            numericInput(ns("fc_min"), "min", value = 0),
                            numericInput(ns("fc_max"), "max", value = 0)
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
                     helpText("En Creation"),
                     # helpText(h3("Distance calculation")),
                     # selectInput(ns('inDist'),"Distance", c("euclidean","maximum",
                     #                                        "manhattan","canberra",
                     #                                        "binary","minkowski"), selected = "binary"),
            )
          ),
        helpText(h3("Hierarchical clustering")),
        selectInput(ns('inHC'),"Clustering hierarchique", c("ward.D","ward.D2",
                                                            "single","complete",
                                                            "average","mcquitty",
                                                            "median","centroid","diana")),
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
      if(typ_data == "Binary"){
          datamat <- r$df()
          datamat <- datamat[rowSums(datamat)>=input$fl_min,]
          if(input$fl_max!=0){
            datamat <- datamat[rowSums(datamat)<=input$flmax,]
          }
          datamat <- datamat[,colSums(datamat)>=input$fc_min]
          if(input$fc_max!=0){
            datamat <- datamat[,colSums(datamat)>=input$fc_max]
          }
      }else{
        #pour donnees numeriques
      }
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
             "S14 Phi of Pearson" = 9,
             "euclidean" = "euclidean",
             "maximum" = "maximum",
            "manhattan" = "manhattan",
            "canberra" = "canberra",
            "binary" = "binary",
            "minkowski" = "minkowski"
      )
    })

    distm <- reactive({
      if(typ_data == "Binary"){
        #pour binaire
      }else{
        #pour numerique
      }
    })




  })
}

## To be copied in the UI
# mod_data_processing_ui("data_processing_1")

## To be copied in the server
# mod_data_processing_server("data_processing_1")
