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
#' @import shinyFeedback
#' @importFrom shiny NS tagList
mod_data_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyFeedback(),
    fluidPage(
      box(title = "Data Processing",status = "primary",solidHeader = TRUE,
          helpText(h4("choose your parameters")),
          helpText(h3("Input matrix processing")),
          radioButtons(ns("typ_data"),"Data type",choices = c( #Ajouter des informations d'aides explications
            "Binary",
            "Numerical"),
            selected = "Binary",inline = TRUE),
          radioButtons(ns("trans"),"Matrix transposition",
                       choiceNames = c("Yes","No"),
                       choiceValues = c(TRUE,FALSE),
                       selected = FALSE,inline = TRUE),
          tags$div(
            "Input matrix :"
          ),
          textOutput(ns("nbRowCol")),
          textOutput(ns("headRow")),
          textOutput(ns("headCol")),

#Ajout mise a jour pour afficher choix pour donner binaire ou donner numerique, voir mastering shiny 10.2.1 Conditional UI
#Tabs pour afficher mode binary ou mode numerical
          parameter_tabs <- tabsetPanel(
            id = ns("params"),
            type = "hidden",
            tabPanel("Binary",
                     helpText(h3("remove lines/columns"),
                          h4("default : 'only 0' and 'only 1'  lines removed. Recommended for most subsequent distance calculation and statistical analysis methods)")
                          ),
                     column(6,
                            numericInput(ns("max_zero_row"), "Maximum number of zero by row", value = 10, min = 0),
                            numericInput(ns("max_one_row"), "Maximum number of one by row", value = 10, min = 0),
                     ),
                     column(6,
                            numericInput(ns("max_zero_col"), "Maximum number of zero by column", value = 10, min = 0),
                            numericInput(ns("max_one_col"), "Maximum number of one by column", value = 10, min = 0),
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
                     helpText(h3("Scaling and Centering")),
                     column(6,
                            radioButtons(ns("d_scale"),"Scale",
                                         choiceNames = c("True","False"),
                                         choiceValues = c(TRUE,FALSE),
                                         selected = FALSE,inline = TRUE),
                     ),
                     column(6,
                            radioButtons(ns("d_center"),"Center",
                                         choiceNames = c("True","False"),
                                         choiceValues = c(TRUE,FALSE),
                                         selected = FALSE,inline = TRUE),
                     ),
                     helpText(h3("Distance calculation")),
                     selectInput(ns('inDist_num'),"Distance", c("euclidean","maximum",
                                                            "manhattan","canberra",
                                                            "binary","minkowski"), selected = "canberra"),
            )
          ),
        helpText(h3("Hierarchical clustering")),
        selectInput(ns('inHC'),"Clustering hierarchique", c("ward.D","ward.D2",
                                                            "single","complete",
                                                            "average","mcquitty",
                                                            "median","centroid","diana"),
                    selected = "ward.D2"),
        helpText(h3("Seriation")),
        radioButtons(ns('ser'),"Seriation (row and/or column optimized ordering)", choices = c("Yes","No"), selected="Yes", inline = TRUE),
        actionButton(ns("val_DP"), "Validate"),
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

    data_tr <- reactive({
      req(r$df)
      datamat <- r$df()
      if(input$trans){
        datamat <- t(datamat)
      }
      return(datamat)
    })

    observeEvent(data_tr(),{
      updateNumericInput(inputId = "max_zero_col", max = nrow(data_tr()), min = 0, value = nrow(data_tr())-1)
      updateNumericInput(inputId = "max_one_col", max = nrow(data_tr()), min = 0, value = nrow(data_tr())-1)
      updateNumericInput(inputId = "max_zero_row", max = ncol(data_tr()), min = 0, value = ncol(data_tr())-1)
      updateNumericInput(inputId = "max_one_row", max = ncol(data_tr()), min = 0, value = ncol(data_tr())-1)
    })

    output$nbRowCol <- renderText({paste("nb row :", nrow(data_tr()), "   nb col :", ncol(data_tr()))})
    output$headRow <- renderText({paste("Row names :", toString(paste(rownames(head(data_tr()))),width = 27))})
    output$headCol <- renderText({paste("Col names :", toString(paste(colnames(head(data_tr()))),width = 27))})

    #mettre les donnees apres filtrage, filtrage en fonction binaire ou non
    r$fil_df <- eventReactive(input$val_DP,{
      req(data_tr)
      hideFeedback(inputId="inDist")
      hideFeedback(inputId="inDist_num")
      hideFeedback(inputId="inHC")
      datamat <- data_tr()
      if(input$typ_data == "Binary"){
        datamat <- datamat[rowSums(datamat==0)<=input$max_zero_row,]
        datamat <- datamat[,colSums(datamat==0)<=input$max_zero_col]
        datamat <- datamat[rowSums(datamat==1)<=input$max_one_row,]
        datamat <- datamat[,colSums(datamat==1)<=input$max_one_col]
      }else{
        #ajouter ici filtre pour numerique
        #Scaling and Centering
        datamat <- scale(datamat, center = as.logical(input$d_center), scale = as.logical(input$d_scale))
      }
      return(datamat)
    })

    meth_dist <- eventReactive(input$val_DP,{
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
    r$distm_ml <- eventReactive(input$val_DP,{
      id <- showNotification("Running row distance... Wait", duration = NULL, closeButton = FALSE, type = "warning")
      on.exit(removeNotification(id), add = TRUE)
      if(input$typ_data == "Binary"){
        tryCatch({
            dMat <- ade4::dist.binary(r$fil_df(), method = meth_dist(), diag = FALSE, upper = FALSE)
            return(dMat)
        }, error = function(e) {
          showFeedback(inputId = "inDist", text = e$message, color = "#d9534f",
                       icon = shiny::icon("exclamation-sign", lib = "glyphicon"),
                       session = shiny::getDefaultReactiveDomain())
        })
      }else{
        tryCatch({
            dMat <- stats::dist(r$fil_df(), method = input$inDist_num)
            return(dMat)
        }, error = function(e) {
          showFeedback(inputId = "inDist_num", text = e$message, color = "#d9534f",
                     icon = shiny::icon("exclamation-sign", lib = "glyphicon"),
                     session = shiny::getDefaultReactiveDomain())
        })
      }
    })

    #distance colonne
    r$distm_mc <- eventReactive(input$val_DP,{
      id <- showNotification("Running column distance... Wait", duration = NULL, closeButton = FALSE, type = "warning")
      on.exit(removeNotification(id), add = TRUE)
      if(input$typ_data == "Binary"){
        tryCatch({
          TdMat <- ade4::dist.binary(t(as.matrix(r$fil_df())), method = meth_dist(), diag = FALSE, upper = FALSE)
          return(TdMat)
        }, error = function(e) {
          showFeedback(inputId = "inDist", text = e$message, color = "#d9534f",
                       icon = shiny::icon("exclamation-sign", lib = "glyphicon"),
                       session = shiny::getDefaultReactiveDomain())
        })
      }else{
        tryCatch({
          TdMat <- stats::dist(t(as.matrix(r$fil_df())), method = input$inDist_num)
          return(TdMat)
        }, error = function(e) {
          showFeedback(inputId = "inDist_num", text = e$message, color = "#d9534f",
                       icon = shiny::icon("exclamation-sign", lib = "glyphicon"),
                       session = shiny::getDefaultReactiveDomain())
        })
      }
    })

    r$HCNotPer_l <- eventReactive(input$val_DP,{
      id <- showNotification("Running row HC... Wait", duration = NULL, closeButton = FALSE, type = "warning")
      on.exit(removeNotification(id), add = TRUE)
      tryCatch({
        if(input$inHC != "diana"){
          HC <- stats::hclust(r$distm_ml(), method= input$inHC)
        } else{
          HC <- stats::as.hclust(cluster::diana(r$distm_ml())) #HC avec diana du package cluster
        }
        return(HC)
      }, error = function(e) {
        showFeedback(inputId = "inHC", text = e$message, color = "#d9534f",
                     icon = shiny::icon("exclamation-sign", lib = "glyphicon"),
                     session = shiny::getDefaultReactiveDomain())
      })
    })

    r$HC_l <- eventReactive(input$val_DP,{
      id <- showNotification("Running row seriation... Wait", duration = NULL, closeButton = FALSE, type = "warning")
      on.exit(removeNotification(id), add = TRUE)
      tryCatch({
        req(r$HCNotPer_l)
        HC <- r$HCNotPer_l()
        if(input$ser=="Yes"){
          OrdSer <- DendSer::DendSer(HC, r$distm_ml(), cost= costARc) #calcul de la seriation avec DendSer du package DendSer
          HC <-  seriation::permute(HC, OrdSer)
        }
        return(HC)
      }, error = function(e) {
        showFeedback(inputId = "inHC", text = paste("Seriation:",e$message,sep=" "), color = "#d9534f",
                     icon = shiny::icon("exclamation-sign", lib = "glyphicon"),
                     session = shiny::getDefaultReactiveDomain())
      })
    })

    r$HCNotPer_c <- eventReactive(input$val_DP,{
      id <- showNotification("Running column HC... Wait", duration = NULL, closeButton = FALSE, type = "warning")
      on.exit(removeNotification(id), add = TRUE)
      tryCatch({
        if(input$inHC != "diana"){
          HC <- stats::hclust(r$distm_mc(), method= input$inHC)
        } else{
          HC <- stats::as.hclust(cluster::diana(r$distm_mc())) #HC avec diana du package cluster
        }
        return(HC)
      }, error = function(e) {
        showFeedback(inputId = "inHC", text = e$message, color = "#d9534f",
                     icon = shiny::icon("exclamation-sign", lib = "glyphicon"),
                     session = shiny::getDefaultReactiveDomain())
      })
    })

    r$HC_c <- eventReactive(input$val_DP,{
      id <- showNotification("Running column seriation... Wait", duration = NULL, closeButton = FALSE, type = "warning")
      on.exit(removeNotification(id), add = TRUE)
      tryCatch({
        req(r$HCNotPer_c)
        HC <- r$HCNotPer_c()
        if(input$ser=="Yes"){
          OrdSer <- DendSer::DendSer(HC, r$distm_mc(), cost= costARc) #calcul de la seriation avec DendSer du package DendSer
          HC <-  seriation::permute(HC, OrdSer)
        }
        return(HC)
      }, error = function(e) {
        showFeedback(inputId = "inHC", text = paste("Seriation:",e$message,sep=" "), color = "#d9534f",
                     icon = shiny::icon("exclamation-sign", lib = "glyphicon"),
                     session = shiny::getDefaultReactiveDomain())
      })
    })

    #matrice ordonnee
    r$M_ser <- eventReactive(input$val_DP,{
      rser <- r$fil_df()
      rser <- rser[seriation::get_order(r$HC_l()), seriation::get_order(r$HC_c())]
    })

    # eventReactive(input$val_DP,{
    #   hideFeedback(inputId="inDist")
    #   hideFeedback(inputId="inDist_num")
    #   hideFeedback(inputId="inHC")
    #   })

    r$typ_data <- reactive({td <- input$typ_data})

  })
}

## To be copied in the UI
# mod_data_processing_ui("data_processing_1")

## To be copied in the server
# mod_data_processing_server("data_processing_1")
